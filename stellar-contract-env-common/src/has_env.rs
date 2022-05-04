use super::Env;

pub trait HasEnv<E: Env> {
    fn env(&self) -> &E;
    fn mut_env(&mut self) -> &mut E;
}
