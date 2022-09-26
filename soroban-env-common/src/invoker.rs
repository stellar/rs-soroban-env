#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
#[repr(u32)]
pub enum InvokerType {
    Account = 0,
    Contract = 1,
}
