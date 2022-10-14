use log::{info, trace};

use optimization::{Func, GradientDescent, Minimizer, NumericalDifferentiation};

pub struct FPPoint {
    pub x: f64,
    pub y: f64,
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd)]
pub struct FPCostModel {
    pub const_param: f64,
    pub log_param: f64,
    pub log_base_param: f64,
    pub lin_param: f64,
    pub quad_param: f64,
}

// We have to use a floating-point cost model in order to interface with the
// numerical optimizer below -- using integral types causes it to get very
// confused due to rounding/truncation.
impl FPCostModel {
    pub fn new(params: &[f64]) -> Self {
        let mut fcm = FPCostModel::default();
        fcm.const_param = params[0];
        fcm.log_param = params[1];
        fcm.log_base_param = params[2];
        fcm.lin_param = params[3];
        fcm.quad_param = params[4];
        fcm
    }
    // This is the same as the 'evaluate' function in the integral cost model,
    // just using f64 ops rather than saturating integer ops.
    pub fn evaluate(&self, input: f64) -> f64 {
        let mut res = self.const_param;
        if input.is_finite() && input != 0.0 {
            res += self.log_param * input.log(self.log_base_param);
            res += self.lin_param * input;
            res += self.quad_param * input * input;
        }
        res
    }
}

// Fits a FloatCostModel to the provided data, using least-squares
// gradient descent optimization.
pub fn fit_model(data: &Vec<FPPoint>) -> FPCostModel {
    // We construct an objective function for the optimizer here that treats its
    // inputs as parametes to a FloatCostModel, and returns the sum (over all
    // provided data points) of squares of differences between the data.y value
    // and the cost model evaluated at the data.x value.
    //
    // In other words, the objective function maps parameters to a measure of
    // goodness-of-fit between the input data and a cost model with the provided
    // parameter values.
    let function = NumericalDifferentiation::new(Func(|params: &[f64]| {
        let cm = FPCostModel::new(params);
        let mut sum_sq = 0.0;
        for pt in data.iter() {
            let eval = cm.evaluate(pt.x);
            let diff = pt.y - eval;
            trace!(
                "evaluated f({}) = {}, data has {}, diff {}",
                pt.x,
                eval,
                pt.y,
                diff
            );
            sum_sq += diff * diff;
        }
        trace!("sum-of-squares of differences at {:?}: {}", cm, sum_sq);
        sum_sq
    }));
    let minimizer = GradientDescent::new().max_iterations(Some(1_000_000));
    let solution = minimizer.minimize(&function, vec![0.0, 0.0, 0.0, 0.0, 0.0]);
    let ret = FPCostModel::new(&solution.position[..]);
    info!("found solution at f({:?}) = {:?}", ret, solution.value);
    ret
}
