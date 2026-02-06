/// This macro relies on `std::any::type_name` string formatting and
/// performs suffix stripping (`::f`, `::{{closure}}`) and path slicing to
/// extract the function name. This depends on compiler-generated type names
/// and is therefore not guaranteed to be stable across compiler versions.
///
/// Intended for test labeling and diagnostics only.
macro_rules! current_function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        let name = name.strip_suffix("::f").unwrap_or(name);
        let name = name.strip_suffix("::{{closure}}").unwrap_or(name);
        let function_name_index = name.rfind("::").map(|index| index + 2).unwrap_or(0);
        &name[function_name_index..]
    }};
}
