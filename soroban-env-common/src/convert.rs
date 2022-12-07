use crate::{Env, IntoVal, TryIntoVal};

/// General trait representing a the ability of some object to perform a
/// (possibly unsuccessful) conversion between two other types.
///
/// This trait acts as an extension point / trait-coherence workaround for impls
/// of `TryIntoVal` that need support from types in downstream crates. The Rust
/// trait-coherence rules mandate that certain impls of `TryIntoVal` must be
/// defined in this crate, in particular `impl TryIntoVal<E, Object> for
/// ScObject` and `impl TryIntoVal<E, ScObject> for Object`. This crate is too
/// early in the crate graph to actually provide those impls, however, so they
/// delegate their implementation to downstream-crate impls of
/// `Convert<ScObject, Object>` and `Convert<Object, ScObject>`, which can be
/// defined by the downstream Host environment.
pub trait Convert<F, T> {
    type Error;
    fn convert(&self, f: F) -> Result<T, Self::Error>;
}

/// A free function that exists strictly for readability in contexts where type
/// ascription is necessary. Exists only to allow you to write
/// `try_convert_to::<Foo>(x,env)` rather than the less-legible
/// `<_ as TryIntoVal<E,Foo>>::try_into_val(x,env)`.
pub fn try_convert_to<V, E: Env, T: TryIntoVal<E, V>>(
    t: T,
    e: &E,
) -> Result<V, <T as TryIntoVal<E, V>>::Error> {
    <T as TryIntoVal<E, V>>::try_into_val(t, e)
}

/// A free function that exists strictly for readability in contexts where type
/// ascription is necessary. Exists only to allow you to write
/// `convert_to::<Foo>(x,env)` rather than the less-legible
/// `<_ as IntoVal<E,Foo>>::try_into_val(x,env)`.
pub fn convert_to<V, E: Env, T: IntoVal<E, V>>(t: T, e: &E) -> V {
    <T as IntoVal<E, V>>::into_val(t, e)
}
