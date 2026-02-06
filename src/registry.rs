use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

/// A registry ID. Always valid against the registry it was returned from.
pub struct Id<T>(usize, PhantomData<T>);

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id(self.0, PhantomData)
    }
}
impl<T> Copy for Id<T> {}

pub fn to_id<T>(raw: usize) -> Id<T> {
    Id(raw, PhantomData)
}

impl<T> Id<T> {
    pub fn raw(&self) -> usize {
        self.0
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id<{}>({})", std::any::type_name::<T>(), self.0)
    }
}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
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
        K: Clone + Debug,
        V: Debug,
    {
        let id = self.entries.len();
        let key_clone = key.clone();
        let mut entry = self.by_name.entry(key).or_default();
        C::insert(&mut entry, id)?;
        self.reverse_lookup.push(key_clone);
        self.entries.push(value);
        Some(to_id(id))
    }

    pub fn get(&self, id: Id<V>) -> Option<&V> {
        self.entries.get(id.raw())
    }

    pub fn get_mut(&mut self, id: Id<V>) -> Option<&mut V> {
        self.entries.get_mut(id.raw())
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
        self.get(self.lookup_id(key)?)
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

    #[allow(dead_code)]
    pub fn entries(&self) -> &Vec<V> {
        &self.entries
    }
}

impl<K, V> Registry<K, V, NoRedefine>
where
    K: Eq + Hash + Debug + Clone,
    V: Debug,
{
    pub fn lookup_or_load<E>(
        &mut self,
        key: K,
        load: impl FnOnce(&K) -> Result<V, E>,
    ) -> Result<Id<V>, E> {
        match self.by_name.get(&key).cloned().flatten() {
            Some(id) => Ok(to_id(id)),
            None => {
                let v = load(&key)?;
                let id = self
                    .insert(key, v)
                    .expect("Insertion always succeeds because entry is vacant");
                Ok(id)
            }
        }
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
