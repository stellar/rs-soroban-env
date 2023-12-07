use nalgebra::{self as na, OMatrix, OVector, U1};
use num_traits::Pow;
use std::collections::HashSet;
use std::str::FromStr;

#[derive(Debug, Default, Clone, PartialEq, PartialOrd)]
pub struct FPCostModel {
    pub const_param: f64,
    pub lin_param: f64,
    pub r_squared: f64,
}

// We have to use a floating-point cost model in order to interface with the
// numerical optimizer below -- using integral types causes it to get very
// confused due to rounding/truncation.
impl FPCostModel {
    pub fn new(params: &[f64], r2: f64) -> Self {
        let mut fcm = FPCostModel::default();
        fcm.const_param = params[0];
        fcm.lin_param = params[1];
        fcm.r_squared = r2;
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
        let extract_param = |f: f64| -> u64 {
            // clamp the float to 1 digit (to filter noise) then take the ceil
            let f = f64::from_str(format!("{:.1}", f).as_str()).unwrap();
            f.ceil() as u64
        };
        (
            extract_param(self.const_param),
            extract_param(self.lin_param),
        )
    }
}

fn compute_rsquared(x: Vec<f64>, y: Vec<f64>, const_param: f64, lin_param: f64) -> f64 {
    assert_eq!(x.len(), y.len());
    let pred_y: Vec<f64> = x.iter().map(|x| const_param + lin_param * x).collect();
    let y_mean = y.iter().sum::<f64>() / y.len() as f64;
    let ss_res = y
        .iter()
        .zip(pred_y.iter())
        .map(|(y, y_pred)| (y - y_pred).pow(2i32))
        .sum::<f64>();
    let ss_tot = y.iter().map(|y| (y - y_mean).pow(2)).sum::<f64>();
    1f64 - ss_res / ss_tot
}

pub fn fit_model(x: Vec<u64>, y: Vec<u64>) -> FPCostModel {
    assert_eq!(x.len(), y.len());
    let const_model = x.iter().collect::<HashSet<_>>().len() == 1;
    if const_model {
        let const_param = y.iter().sum::<u64>() as f64 / y.len() as f64;
        return FPCostModel {
            const_param,
            lin_param: 0.0,
            r_squared: 0.0, // we are always predicting the mean
        };
    }

    let x: Vec<f64> = x.iter().map(|i| *i as f64).collect();
    let y: Vec<f64> = y.iter().map(|i| *i as f64).collect();

    // First pass: try to pin the solution to (x0, y(x=x0)), where x0 is the
    // smallest input in the input range X, assuming X is monotonic increasing.
    // x0 is not necessary equal to 0. Often times it is unrealistic to build a
    // sample with input at exactly zero (e.g you can't deserialize a zero byte
    // blob to XDR). Here we try to pin it to the lowest point to ensure the
    // y-intercept of the produced curve is sane.
    assert!(y.len() > 1 && x.len() > 1);
    let x0 = x.get(0).unwrap();
    let y0 = y.get(0).unwrap();
    // we build the matrix a and b, the independent and dependent variables in
    // the equation to be optimized
    let a: Vec<f64> = x.iter().map(|x| x - x0).collect();
    let a = OMatrix::<f64, na::Dyn, U1>::from_column_slice(&a);
    let b: Vec<f64> = y.iter().map(|y| y - y0).collect();
    let b = OVector::<f64, na::Dyn>::from_row_slice(&b);
    // computes the least-square solution with a small tolerance
    let lsq_res = lstsq::lstsq(&a, &b, 1e-14).unwrap();
    assert_eq!(lsq_res.solution.len(), 1);

    let lin_param = *lsq_res.solution.get(0).unwrap();
    assert!(
        lin_param >= 0.0,
        "{}",
        format!(
            "negative slope {} detected, examine your data, or choose a constant model",
            lin_param
        )
    );
    let const_param = y0 - lin_param * x0;
    if const_param >= 0.0 {
        // we have found our solution: the line is least-square minimal, **and**
        // the intercept is non-negative
        let r_squared = compute_rsquared(x.clone(), y.clone(), const_param, lin_param);
        return FPCostModel {
            const_param,
            lin_param,
            r_squared,
        };
    }

    // negative intercept means that extrapolating our solution to the range of
    // [0, x0), will produce a negative y for some values. This is unaceptable
    // because someone can pass in an input that produces negative cost.
    println!(
        "negative intercept detected, will constrain the solution to pass through (0,0) and rerun"
    );
    let a = OMatrix::<f64, na::Dyn, U1>::from_column_slice(&x);
    let b = OVector::<f64, na::Dyn>::from_row_slice(&y);
    let lsq_res = lstsq::lstsq(&a, &b, 1e-14).unwrap();
    assert_eq!(lsq_res.solution.len(), 1);
    let lin_param = *lsq_res.solution.get(0).unwrap();
    let r_squared = compute_rsquared(x.clone(), y.clone(), 0.0, lin_param);
    FPCostModel {
        const_param: 0.0,
        lin_param,
        r_squared,
    }
}
