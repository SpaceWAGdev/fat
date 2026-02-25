pub mod latex;

pub trait AsLaTeX {
    fn as_latex(&self) -> anyhow::Result<String>;
}
pub trait AsTypst {
    fn as_latex(&self) -> anyhow::Result<String>;
}
