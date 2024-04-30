#![allow(dead_code)]

use blst::{
    blst_final_exp, blst_fp, blst_fp12, blst_fp2, blst_fp6, blst_fp_from_lendian, blst_fr,
    blst_fr_from_scalar, blst_hash_to_g1, blst_hash_to_g2, blst_lendian_from_scalar,
    blst_map_to_g1, blst_map_to_g2, blst_miller_loop, blst_p1, blst_p1_add, blst_p1_affine,
    blst_p1_affine_in_g1, blst_p1_deserialize, blst_p1_from_affine, blst_p1_in_g1, blst_p1_mult,
    blst_p1_serialize, blst_p2, blst_p2_add, blst_p2_affine, blst_p2_affine_in_g2,
    blst_p2_deserialize, blst_p2_from_affine, blst_p2_in_g2, blst_p2_mult, blst_p2_serialize,
    blst_scalar, blst_scalar_fr_check, blst_scalar_from_fr, blst_scalar_from_lendian, BLST_ERROR,
};

use crate::{
    xdr::{ScErrorCode, ScErrorType},
    Host, HostError, Val,
};

pub const BLS_G1_UNCOMPRESSED_SIZE: usize = 96;
pub const BLS_G2_UNCOMPRESSED_SIZE: usize = 192;
pub const BLS_FP_SIZE: usize = 48;
pub const BLS_SCALAR_SIZE: usize = 32;
pub const BLS_RESULT_SIZE: usize = 255;
pub const BLS_G1_DST: &[u8; 50] = b"QUUX-V01-CS02-with-BLS12381G1_XMD:SHA-256_SSWU_RO_";
pub const BLS_G2_DST: &[u8; 50] = b"QUUX-V01-CS02-with-BLS12381G2_XMD:SHA-256_SSWU_RO_";

const BLS_FP12_ZERO: blst_fp12 = blst_fp12 {
    fp6: [blst_fp6 {
        fp2: [blst_fp2 {
            fp: [blst_fp { l: [0; 6] }; 2],
        }; 3],
    }; 2],
};

impl Host {
    pub(crate) fn bls_g1_add_raw_internal(
        &self,
        p0: &[u8; BLS_G1_UNCOMPRESSED_SIZE],
        p1: &[u8; BLS_G1_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLS_G1_UNCOMPRESSED_SIZE], HostError> {
        let p0 = self.parse_point_in_g1(p0)?;
        let p1 = self.parse_point_in_g1(p1)?;

        let mut res = blst_p1::default();
        let mut out = [0u8; BLS_G1_UNCOMPRESSED_SIZE];

        unsafe { blst_p1_add(&mut res, &p0, &p1) };

        unsafe {
            blst_p1_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }

    pub(crate) fn bls_g1_mul_raw_internal(
        &self,
        scalar: &[u8; BLS_SCALAR_SIZE],
        p1: &[u8; BLS_G1_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLS_G1_UNCOMPRESSED_SIZE], HostError> {
        let p1 = self.parse_point_in_g1(p1)?;
        let scalar = self.parse_scalar(scalar)?;
        let mut res = blst_p1::default();
        let mut out = [0u8; BLS_G1_UNCOMPRESSED_SIZE];
        unsafe { blst_p1_mult(&mut res, &p1, scalar.as_ptr(), BLS_RESULT_SIZE) };
        unsafe {
            blst_p1_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }
    pub(crate) fn bls_g1_multiexp_raw_internal(
        &self,
        scalars: &[u8],
        p_n: &[u8],
    ) -> Result<[u8; BLS_G1_UNCOMPRESSED_SIZE], HostError> {
        if let Some(value) = self.validate_points_input(&p_n, BLS_G1_UNCOMPRESSED_SIZE) {
            return Err(value);
        }
        if let Some(value) = self.validate_points_input(&scalars, BLS_SCALAR_SIZE) {
            return Err(value);
        }

        let mut res = blst_p1::default();
        let mut out = [0u8; BLS_G1_UNCOMPRESSED_SIZE];
        let scalars = scalars.chunks_exact(BLS_SCALAR_SIZE);
        for (chunk, scalar) in p_n.chunks_exact(BLS_G1_UNCOMPRESSED_SIZE).zip(scalars) {
            let p1 = self.parse_point_in_g1(chunk.try_into().unwrap())?;
            let mut tmp = blst_p1::default();
            let scalar = self.parse_scalar(scalar.try_into().unwrap())?;
            unsafe { blst_p1_mult(&mut tmp, &p1, scalar.as_ptr(), BLS_RESULT_SIZE) };
            unsafe { blst_p1_add(&mut res, &res, &tmp) };
        }

        unsafe {
            blst_p1_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }

    pub(crate) fn bls_g2_add_raw_internal(
        &self,
        p0: &[u8; BLS_G2_UNCOMPRESSED_SIZE],
        p1: &[u8; BLS_G2_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLS_G2_UNCOMPRESSED_SIZE], HostError> {
        let p0 = self.parse_point_in_g2(p0)?;
        let p1 = self.parse_point_in_g2(p1)?;
        let mut res = blst_p2::default();
        let mut out = [0u8; BLS_G2_UNCOMPRESSED_SIZE];

        unsafe { blst_p2_add(&mut res, &p0, &p1) };

        unsafe {
            blst_p2_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }

    pub(crate) fn bls_g2_mul_raw_internal(
        &self,
        scalar: &[u8; BLS_SCALAR_SIZE],
        p1: &[u8; BLS_G2_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLS_G2_UNCOMPRESSED_SIZE], HostError> {
        let p1 = self.parse_point_in_g2(p1)?;
        let scalar = self.parse_scalar(&scalar)?;
        let mut res = blst_p2::default();
        let mut out = [0u8; BLS_G2_UNCOMPRESSED_SIZE];
        unsafe { blst_p2_mult(&mut res, &p1, scalar.as_ptr(), BLS_RESULT_SIZE) };

        unsafe {
            blst_p2_serialize(out.as_mut_ptr(), &res);
        }
        Ok(out)
    }

    pub(crate) fn bls_g2_multiexp_raw_internal(
        &self,
        scalars: &[u8],
        p_n: &[u8],
    ) -> Result<[u8; BLS_G2_UNCOMPRESSED_SIZE], HostError> {
        if let Some(value) = self.validate_points_input(&p_n, BLS_G2_UNCOMPRESSED_SIZE) {
            return Err(value);
        }
        if let Some(value) = self.validate_points_input(&scalars, BLS_SCALAR_SIZE) {
            return Err(value);
        }

        let mut res = blst_p2::default();
        let mut out = [0u8; BLS_G2_UNCOMPRESSED_SIZE];
        let scalars = scalars.chunks_exact(BLS_SCALAR_SIZE);
        for (chunk, scalar) in p_n.chunks_exact(BLS_G1_UNCOMPRESSED_SIZE).zip(scalars) {
            let p2 = self.parse_point_in_g2(chunk.try_into().unwrap())?;
            let mut tmp = blst_p2::default();
            let scalar = self.parse_scalar(scalar.try_into().unwrap())?;
            unsafe { blst_p2_mult(&mut tmp, &p2, scalar.as_ptr(), BLS_RESULT_SIZE) };
            unsafe { blst_p2_add(&mut res, &res, &tmp) };
        }
        unsafe {
            blst_p2_serialize(out.as_mut_ptr(), &res);
        }

        Ok(out)
    }

    pub(crate) fn bls_map_to_g1_internal(
        &self,
        fp: &[u8; BLS_FP_SIZE * 2],
    ) -> Result<[u8; BLS_G1_UNCOMPRESSED_SIZE], HostError> {
        let mut out = [0u8; BLS_G1_UNCOMPRESSED_SIZE];
        let (u_fp, v_fp) = fp.split_at(BLS_FP_SIZE);
        let mut g1_point = blst_p1::default();
        let mut u_res = blst_fp::default();
        let mut v_res = blst_fp::default();
        unsafe {
            blst_fp_from_lendian(&mut u_res, u_fp.as_ptr());
            blst_fp_from_lendian(&mut v_res, v_fp.as_ptr());
            blst_map_to_g1(&mut g1_point, &u_res, &v_res);
            blst_p1_serialize(out.as_mut_ptr(), &g1_point);
        }
        Ok(out)
    }

    pub(crate) fn bls_map_to_g2_internal(
        &self,
        fp2: &[u8; BLS_FP_SIZE * 4],
    ) -> Result<[u8; BLS_G2_UNCOMPRESSED_SIZE], HostError> {
        let mut g2_point = blst_p2::default();
        let mut u_res = blst_fp2::default();
        let mut v_res = blst_fp2::default();

        let (u_fp2, v_fp2) = fp2.split_at(BLS_FP_SIZE * 2);
        let (u0, u1) = u_fp2.split_at(BLS_FP_SIZE);
        let (v0, v1) = v_fp2.split_at(BLS_FP_SIZE);

        unsafe { blst_fp_from_lendian(&mut u_res.fp[0], u0.as_ptr()) };
        unsafe { blst_fp_from_lendian(&mut u_res.fp[1], u1.as_ptr()) };
        unsafe { blst_fp_from_lendian(&mut v_res.fp[0], v0.as_ptr()) };
        unsafe { blst_fp_from_lendian(&mut v_res.fp[1], v1.as_ptr()) };

        let mut out = [0u8; BLS_G2_UNCOMPRESSED_SIZE];
        unsafe {
            blst_map_to_g2(&mut g2_point, &u_res, &v_res);
            blst_p2_serialize(out.as_mut_ptr(), &g2_point);
        }

        Ok(out)
    }

    pub(crate) fn bls_pairing_internal(
        &self,
        g1_point: &[u8; BLS_G1_UNCOMPRESSED_SIZE],
        g2_point: &[u8; BLS_G2_UNCOMPRESSED_SIZE],
    ) -> Result<[u8; BLS_FP_SIZE * 12], HostError> {
        let mut tmp = blst_fp12::default();

        let mut out = blst_fp12::default();
        let p1 = decode_p1_affine(g1_point).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "Failed to decode G1 point",
                &[],
            )
        })?;
        let p2 = decode_p2_affine(g2_point).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "Failed to decode G2 point",
                &[],
            )
        })?;

        unsafe {
            blst_miller_loop(&mut tmp, &p2, &p1);
            blst_final_exp(&mut out, &tmp);
        };
        return Ok(out.to_bendian());
    }

    pub(crate) fn bls_hash_to_g1_internal(
        &self,
        msg: &[u8],
    ) -> Result<[u8; BLS_G1_UNCOMPRESSED_SIZE], HostError> {
        let mut res = blst_p1::default();
        unsafe {
            blst_hash_to_g1(
                &mut res,
                msg.as_ptr(),
                msg.len(),
                BLS_G1_DST.as_ptr(),
                BLS_G1_DST.len(),
                [].as_ptr(),
                0,
            );
        }
        let mut out = [0u8; BLS_G1_UNCOMPRESSED_SIZE];
        unsafe { blst_p1_serialize(out.as_mut_ptr(), &res) };
        Ok(out)
    }

    pub(crate) fn bls_hash_to_g2_internal(
        &self,
        msg: &[u8],
    ) -> Result<[u8; BLS_G2_UNCOMPRESSED_SIZE], HostError> {
        let mut res = blst_p2::default();
        unsafe {
            blst_hash_to_g2(
                &mut res,
                msg.as_ptr(),
                msg.len(),
                BLS_G2_DST.as_ptr(),
                BLS_G2_DST.len(),
                [].as_ptr(),
                0,
            );
        }
        let mut out = [0u8; BLS_G2_UNCOMPRESSED_SIZE];
        unsafe { blst_p2_serialize(out.as_mut_ptr(), &res) };
        Ok(out)
    }

    fn validate_points_input(&self, p_n: &&[u8], size: usize) -> Option<HostError> {
        if p_n.len() % size != 0 {
            return Some(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("number of points bytes should divisible by {}", size).as_str(),
                &[Val::from_u32(p_n.len() as u32).into()],
            ));
        }
        None
    }

    fn parse_point_in_g1(&self, p1: &[u8; BLS_G1_UNCOMPRESSED_SIZE]) -> Result<blst_p1, HostError> {
        decode_p1(p1).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "Failed to decode point in g1",
                &[],
            )
        })
    }

    fn parse_point_in_g2(&self, p2: &[u8; BLS_G2_UNCOMPRESSED_SIZE]) -> Result<blst_p2, HostError> {
        decode_p2(p2).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "Failed to decode point in g2",
                &[],
            )
        })
    }

    fn parse_scalar(
        &self,
        scalar: &[u8; BLS_SCALAR_SIZE],
    ) -> Result<[u8; BLS_SCALAR_SIZE], HostError> {
        let scalar_ftr = scalar_fr_from_bytes(scalar).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "Failed to decode scalar",
                &[],
            )
        })?;

        bls_fr_to_bytes(scalar_ftr).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "Failed to convert scalar",
                &[],
            )
        })
    }
}

pub fn g1_one() -> [u8; BLS_G1_UNCOMPRESSED_SIZE] {
    unsafe { serialize_default_p1(&blst::BLS12_381_G1) }
}

pub fn g1_zero() -> [u8; BLS_G1_UNCOMPRESSED_SIZE] {
    let mut zero = [0u8; BLS_G1_UNCOMPRESSED_SIZE];
    zero[0] = 0x40;
    zero
}

pub fn g2_one() -> [u8; BLS_G2_UNCOMPRESSED_SIZE] {
    unsafe { serialize_default_p2(&blst::BLS12_381_G2) }
}

pub fn g2_zero() -> [u8; BLS_G2_UNCOMPRESSED_SIZE] {
    let mut zero = [0u8; BLS_G2_UNCOMPRESSED_SIZE];
    zero[0] = 0x40;
    zero
}

pub fn bls_fr_to_bytes(scalar_fr: blst_fr) -> Option<[u8; BLS_SCALAR_SIZE]> {
    let mut out = [0u8; BLS_SCALAR_SIZE];
    unsafe {
        let mut scalar = blst_scalar::default();
        blst_scalar_from_fr(&mut scalar, &scalar_fr);
        blst_lendian_from_scalar(out.as_mut_ptr(), &scalar);
    }
    Some(out)
}

pub fn scalar_fr_from_bytes(bytes: &[u8; BLS_SCALAR_SIZE]) -> Option<blst_fr> {
    unsafe {
        let mut scalar = blst_scalar::default();
        blst_scalar_from_lendian(&mut scalar, bytes.as_ptr());
        blst_scalar_fr_check(&scalar).then(|| {
            let mut fr = blst_fr::default();
            blst_fr_from_scalar(&mut fr, &scalar);
            fr
        })
    }
}

pub fn decode_p1_affine(bytes: &[u8; BLS_G1_UNCOMPRESSED_SIZE]) -> Option<blst_p1_affine> {
    unsafe {
        let mut raw = blst_p1_affine::default();
        (blst_p1_deserialize(&mut raw, bytes.as_ptr()) == BLST_ERROR::BLST_SUCCESS
            && blst_p1_affine_in_g1(&raw))
        .then_some(raw)
    }
}

pub fn decode_p1(bytes: &[u8; BLS_G1_UNCOMPRESSED_SIZE]) -> Option<blst_p1> {
    decode_p1_affine(bytes).and_then(|p1_affine| unsafe {
        let mut raw = blst_p1::default();
        blst_p1_from_affine(&mut raw, &p1_affine);
        blst_p1_in_g1(&raw).then_some(raw)
    })
}

pub fn decode_p2_affine(bytes: &[u8; BLS_G2_UNCOMPRESSED_SIZE]) -> Option<blst_p2_affine> {
    unsafe {
        let mut raw = blst_p2_affine::default();
        (blst_p2_deserialize(&mut raw, bytes.as_ptr()) == BLST_ERROR::BLST_SUCCESS
            && blst_p2_affine_in_g2(&raw))
        .then_some(raw)
    }
}

pub fn decode_p2(bytes: &[u8; BLS_G2_UNCOMPRESSED_SIZE]) -> Option<blst_p2> {
    decode_p2_affine(bytes).and_then(|p2_affine| unsafe {
        let mut raw = blst_p2::default();
        blst_p2_from_affine(&mut raw, &p2_affine);
        blst_p2_in_g2(&raw).then_some(raw)
    })
}

unsafe fn serialize_default_p1(affine: &blst_p1_affine) -> [u8; BLS_G1_UNCOMPRESSED_SIZE] {
    let mut out = [0u8; BLS_G1_UNCOMPRESSED_SIZE];
    let mut raw = blst_p1::default();
    blst_p1_from_affine(&mut raw, affine);
    blst_p1_serialize(out.as_mut_ptr(), &raw);
    out
}

unsafe fn serialize_default_p2(affine: &blst_p2_affine) -> [u8; BLS_G2_UNCOMPRESSED_SIZE] {
    let mut out = [0u8; BLS_G2_UNCOMPRESSED_SIZE];
    let mut raw = blst_p2::default();
    blst_p2_from_affine(&mut raw, affine);
    blst_p2_serialize(out.as_mut_ptr(), &raw);
    out
}
