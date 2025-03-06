pub trait VecExtensions<T> {
    /// Removes the first `n` elements from the vector and returns them as a new vector.
    ///
    /// If `n` is greater than the length of the vector, the whole vector is drained.
    fn pop_up_to_n(&mut self, n: usize) -> Vec<T>;
}

impl<T> VecExtensions<T> for Vec<T> {
    fn pop_up_to_n(&mut self, n: usize) -> Vec<T> {
        let num_elements_to_pop = if n <= self.len() { n } else { self.len() };
        self.drain(..num_elements_to_pop).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pop_zero_element_return_empty_vec() {
        let mut vec = vec![1, 2, 3];
        let popped = vec.pop_up_to_n(0);

        assert_eq!(popped, Vec::<i32>::new());
        assert_eq!(vec, vec![1, 2, 3]);
    }

    #[test]
    fn pop_one_element_return_vec_with_the_first_element() {
        let mut vec = vec![1, 2, 3];
        let popped = vec.pop_up_to_n(1);

        assert_eq!(popped, vec![1]);
        assert_eq!(vec, vec![2, 3]);
    }

    #[test]
    fn pop_all_elements_leave_source_vec_empty() {
        let mut vec = vec![1, 2, 3];
        let popped = vec.pop_up_to_n(vec.len());

        assert_eq!(popped, vec![1, 2, 3]);
        assert_eq!(vec, Vec::<i32>::new());
    }

    #[test]
    fn pop_more_than_source_vec_size_is_equivalent_to_popping_all_elements() {
        let mut vec = vec![1, 2, 3];
        let popped = vec.pop_up_to_n(vec.len() + 1);

        assert_eq!(popped, vec![1, 2, 3]);
        assert_eq!(vec, Vec::<i32>::new());
    }
}
