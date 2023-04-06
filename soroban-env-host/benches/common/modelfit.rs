use std::collections::HashSet;

use log::info;

use linregress::{FormulaRegressionBuilder, RegressionDataBuilder};

#[derive(Debug)]
pub struct FPPoint {
    pub x: f64,
    pub y: f64,
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd)]
pub struct FPCostModel {
    pub const_param: f64,
    pub lin_param: f64,
}

// We have to use a floating-point cost model in order to interface with the
// numerical optimizer below -- using integral types causes it to get very
// confused due to rounding/truncation.
impl FPCostModel {
    pub fn new(params: &[f64]) -> Self {
        let mut fcm = FPCostModel::default();
        fcm.const_param = params[0];
        fcm.lin_param = params[1];
        fcm
    }
    // This is the same as the 'evaluate' function in the integral cost model,
    // just using f64 ops rather than saturating integer ops.
    pub fn evaluate(&self, input: f64) -> f64 {
        let mut res = self.const_param;
        if input.is_finite() && input != 0.0 {
            res += self.lin_param * input;
        }
        res
    }
    // Extract the parameters from FPs to integers
    pub fn params_as_u64(&self) -> (u64, u64) {
        (
            self.const_param.max(0.0).ceil() as u64,
            self.lin_param.max(0.0).ceil() as u64,
        )
    }
}

fn fit_linear_regression(x: Vec<f64>, y: Vec<f64>) -> FPCostModel {
    let data = vec![("Y", y), ("X", x)];
    let data = RegressionDataBuilder::new().build_from(data).unwrap();
    let model = FormulaRegressionBuilder::new()
        .data(&data)
        .formula("Y ~ X")
        .fit()
        .unwrap();
    info!(
        "Linear regression found parameters {:?}, with R2 = {}",
        model.parameters(),
        model.rsquared()
    );
    FPCostModel::new(model.parameters())
}

pub fn fit_model(x: Vec<u64>, y: Vec<u64>) -> FPCostModel {
    let const_model = x.iter().collect::<HashSet<_>>().len() == 1;
    if const_model {
        let const_param = y.iter().sum::<u64>() as f64 / y.len() as f64;
        return FPCostModel {
            const_param,
            lin_param: 0.0,
        };
    }

    let x = x.iter().map(|i| *i as f64).collect::<Vec<_>>();
    let y = y.iter().map(|i| *i as f64).collect::<Vec<_>>();
    fit_linear_regression(x, y)
}
