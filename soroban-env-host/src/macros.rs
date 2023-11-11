// This module contains ubiquitous macros that get used in many other modules
// in the host crate.

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

#[cfg(feature = "testutils")]
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
