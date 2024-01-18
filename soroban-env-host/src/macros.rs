// This module contains ubiquitous macros that get used in many other modules
// in the host crate. This module is ordered before any other module in the
// crate to ensure availability of the macros using #[macro_use]. The other
// option is to add weird `pub(crate) use foo` statements after each definition
// and import them as items, which is a more modern style, but we avoid it here
// and use the older style for simplicity.

#[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
macro_rules! tracy_span {
    () => {
        tracy_client::span!()
    };
    ($name:expr) => {
        tracy_client::span!($name)
    };
}

#[cfg(any(target_family = "wasm", not(feature = "tracy")))]
macro_rules! tracy_span {
    () => {
        ()
    };
    ($name:expr) => {
        ()
    };
}

#[cfg(test)]
macro_rules! function_name {
    () => {{
        // This is the canonical hack used to get a function's name,
        // copied from the stdext crate (and stackoverflow).
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        // `3` is the length of the `::f`.
        &name[..name.len() - 3]
    }};
}

macro_rules! function_short_name {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        // `3` is the length of the `::f`.
        let name = &name[..name.len() - 3];
        &name[name.rfind("::").map(|x| x + 2).unwrap_or(0)..]
    }};
}

#[cfg(test)]
macro_rules! observe_host {
    ($host:expr) => {
        $crate::test::observe::ObservedHost::new(function_name!(), $host)
    };
}

macro_rules! host_vec {
    ($host:expr $(,)?) => {
        $crate::builtin_contracts::base_types::Vec::new($host)
    };
    ($host:expr, $($x:expr),+ $(,)?) => {
        $crate::builtin_contracts::base_types::Vec::from_slice($host, &[$($x.try_into_val($host)?),+])
    };
}

// This is just a variant of `host_vec` that unwraps args and the result
#[cfg(test)]
macro_rules! test_vec {
    ($host:expr $(,)?) => {
        $crate::builtin_contracts::base_types::Vec::new($host).unwrap()
    };
    ($host:expr, $($x:expr),+ $(,)?) => {
        $crate::builtin_contracts::base_types::Vec::from_slice($host, &[$($x.try_into_val($host).unwrap()),+]).unwrap()
    };
}
