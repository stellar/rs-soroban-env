/// ShallowEq defines types that can be shallow compared, where the value of the
/// type is equal or not, but there is no indication with that comparison if the
/// types refer to values in the host that are equal.
pub trait ShallowEq {
    fn shallow_eq(&self, other: &Self) -> bool;
}
