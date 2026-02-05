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
