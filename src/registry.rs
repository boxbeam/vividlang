use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;

/// A registry ID. Always valid against the registry it was returned from.
#[derive(Clone, Copy)]
pub struct Id<T>(usize, PhantomData<T>);

fn to_id<T>(raw: usize) -> Id<T> {
    Id(raw, PhantomData)
}

impl<T> Id<T> {
    pub fn raw(&self) -> usize {
        self.0
    }
}

/// Append-only registry which allows different strategies for handling name collisions.
/// Entries may be unbound from their names, but never removed from the underlying memory,
/// ensuring IDs returned from the registry are always valid against it.
pub struct Registry<K, V, C = NoRedefine>
where
    K: Hash + Eq,
    C: CollisionStrategy,
{
    entries: Vec<V>,
    reverse_lookup: Vec<K>,
    by_name: HashMap<K, C::Storage>,
}

impl<K: Hash + Eq, V, C: CollisionStrategy> Default for Registry<K, V, C> {
    fn default() -> Self {
        Self {
            entries: vec![],
            reverse_lookup: vec![],
            by_name: HashMap::new(),
        }
    }
}

pub trait CollisionStrategy {
    type Storage: Default;

    fn insert(storage: &mut Self::Storage, id: usize) -> Option<()>;
    fn unbind(storage: &mut Self::Storage);
    fn get(storage: &Self::Storage) -> Option<usize>;
}

impl<K: Hash + Eq, V, C: CollisionStrategy> Registry<K, V, C> {
    pub fn insert(&mut self, key: K, value: V) -> Option<Id<V>>
    where
        K: Clone,
    {
        let id = self.entries.len();
        let key_clone = key.clone();
        let mut entry = self.by_name.entry(key).or_default();
        C::insert(&mut entry, id)?;
        self.reverse_lookup.push(key_clone);
        self.entries.push(value);
        Some(to_id(id))
    }

    pub fn get(&self, id: usize) -> Option<&V> {
        self.entries.get(id)
    }

    pub fn get_mut(&mut self, id: usize) -> Option<&mut V> {
        self.entries.get_mut(id)
    }

    pub fn key(&self, id: usize) -> &K {
        &self.reverse_lookup[id]
    }

    pub fn lookup_id<Q: ?Sized>(&self, key: &Q) -> Option<Id<V>>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let entry = self.by_name.get(key)?;
        C::get(entry).map(to_id)
    }

    pub fn lookup<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.get(self.lookup_id(key)?.raw())
    }

    pub fn lookup_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let id = self.lookup_id(key)?.raw();
        self.entries.get_mut(id)
    }

    /// Unbinds a name from the registry, but does not remove the entry
    pub fn unbind<Q: ?Sized>(&mut self, key: &Q)
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let Some(entry) = self.by_name.get_mut(key) else {
            return;
        };
        C::unbind(entry);
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn entries(&self) -> &[V] {
        &*self.entries
    }

    pub fn keys(&self) -> &[K] {
        &*self.reverse_lookup
    }
}

/// Attempting to redefine an already-defined name will result in an error.
pub struct NoRedefine;
/// Attempting to redefine an already-defined name will shadow the previous value.
/// If the name is later unbound, the previous value will be restored.
pub struct Shadowing;

impl CollisionStrategy for NoRedefine {
    type Storage = Option<usize>;

    fn insert(storage: &mut Self::Storage, id: usize) -> Option<()> {
        if storage.is_some() {
            return None;
        }
        storage.replace(id);
        Some(())
    }

    fn get(storage: &Self::Storage) -> Option<usize> {
        storage.clone()
    }

    fn unbind(storage: &mut Self::Storage) {
        storage.take();
    }
}

impl CollisionStrategy for Shadowing {
    type Storage = Vec<usize>;

    fn insert(storage: &mut Self::Storage, id: usize) -> Option<()> {
        storage.push(id);
        Some(())
    }

    fn get(storage: &Self::Storage) -> Option<usize> {
        storage.last().copied()
    }

    fn unbind(storage: &mut Self::Storage) {
        storage.pop();
    }
}
