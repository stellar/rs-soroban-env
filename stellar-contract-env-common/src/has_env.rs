use super::Env;

pub trait HasEnv<E: Env> {
    fn env(&self) -> &E;
}
