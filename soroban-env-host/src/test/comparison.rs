use crate::xdr::ScVal;
use crate::{Compare, Host, RawVal, Tag, TryFromVal};
use itertools::Itertools;

/// Test that comparison of an object of one type to a small value of another
/// type produces the same results as the equivalent ScVal comparison.
///
/// This is a test of the Host::obj_cmp and Tag::get_scval_type methods.
///
/// It works by generating an "example" RawVal for every possible tag,
/// with a match on Tag that ensures it will be updated as Tag changes.
///
/// Those examples are then converted to an array of ScVal.
///
/// For both arrays, every pairwise comparison is performed, and must be equal.
#[test]
fn compare_obj_to_small() {
    let host = Host::default();
    let rawvals: Vec<RawVal> = all_tags()
        .into_iter()
        .map(|t| example_for_tag(&host, t))
        .collect();
    let scvals: Vec<ScVal> = rawvals
        .iter()
        .map(|r| ScVal::try_from_val(&host, r).expect("scval"))
        .collect();

    let rawval_pairs = rawvals.iter().cartesian_product(&rawvals);
    let scval_pairs = scvals.iter().cartesian_product(&scvals);

    let pair_pairs = rawval_pairs.zip(scval_pairs);

    for ((rawval1, rawval2), (scval1, scval2)) in pair_pairs {
        let rawval_cmp = host.compare(rawval1, rawval2).expect("compare");
        let scval_cmp = scval1.cmp(scval2);
        assert_eq!(rawval_cmp, scval_cmp);
    }
}

fn all_tags() -> Vec<Tag> {
    (0_u8..=255)
        .map(Tag::from_u8)
        .filter(|t| {
            // bad tags can't be converted to ScVal
            !matches!(t, Tag::Bad)
        })
        .filter(|t| {
            // objects of this type can't be instantiated
            !matches!(t, Tag::LedgerKeyNonceObject)
        })
        .collect()
}

fn example_for_tag(host: &Host, tag: Tag) -> RawVal {
    use crate::{xdr, Status};

    let ex = match tag {
        Tag::False => RawVal::from(false),
        Tag::True => RawVal::from(true),
        Tag::Void => RawVal::from(()),
        Tag::Status => RawVal::from(Status::from(xdr::ScStatus::Ok)),
        Tag::U32Val => RawVal::from(u32::MAX),
        Tag::I32Val => RawVal::from(i32::MAX),
        Tag::U64Small => RawVal::try_from_val(host, &0_u64).unwrap(),
        Tag::I64Small => RawVal::try_from_val(host, &0_i64).unwrap(),
        Tag::TimepointSmall => {
            RawVal::try_from_val(host, &ScVal::Timepoint(xdr::TimePoint(0))).unwrap()
        }
        Tag::DurationSmall => {
            RawVal::try_from_val(host, &ScVal::Duration(xdr::Duration(0))).unwrap()
        }
        Tag::U128Small => RawVal::try_from_val(host, &0_u128).unwrap(),
        Tag::I128Small => RawVal::try_from_val(host, &0_i128).unwrap(),
        Tag::U256Small => RawVal::try_from_val(
            host,
            &ScVal::U256(xdr::UInt256Parts {
                hi_hi: 0,
                hi_lo: 0,
                lo_hi: 0,
                lo_lo: 0,
            }),
        )
        .unwrap(),
        Tag::I256Small => RawVal::try_from_val(
            host,
            &ScVal::I256(xdr::Int256Parts {
                hi_hi: 0,
                hi_lo: 0,
                lo_hi: 0,
                lo_lo: 0,
            }),
        )
        .unwrap(),
        Tag::SymbolSmall => {
            RawVal::try_from_val(host, &ScVal::Symbol(xdr::ScSymbol::try_from("").unwrap()))
                .unwrap()
        }
        Tag::LedgerKeyContractExecutable => {
            RawVal::try_from_val(host, &ScVal::LedgerKeyContractExecutable).unwrap()
        }
        Tag::SmallCodeUpperBound => panic!(),
        Tag::ObjectCodeLowerBound => panic!(),
        Tag::U64Object => RawVal::try_from_val(host, &u64::MAX).unwrap(),
        Tag::I64Object => RawVal::try_from_val(host, &i64::MAX).unwrap(),
        Tag::TimepointObject => {
            RawVal::try_from_val(host, &ScVal::Timepoint(xdr::TimePoint(u64::MAX))).unwrap()
        }
        Tag::DurationObject => {
            RawVal::try_from_val(host, &ScVal::Duration(xdr::Duration(u64::MAX))).unwrap()
        }
        Tag::U128Object => RawVal::try_from_val(host, &u128::MAX).unwrap(),
        Tag::I128Object => RawVal::try_from_val(host, &i128::MAX).unwrap(),
        Tag::U256Object => RawVal::try_from_val(
            host,
            &ScVal::U256(xdr::UInt256Parts {
                hi_hi: u64::MAX,
                hi_lo: u64::MAX,
                lo_hi: u64::MAX,
                lo_lo: u64::MAX,
            }),
        )
        .unwrap(),
        Tag::I256Object => RawVal::try_from_val(
            host,
            &ScVal::I256(xdr::Int256Parts {
                hi_hi: i64::MIN,
                hi_lo: u64::MAX,
                lo_hi: u64::MAX,
                lo_lo: u64::MAX,
            }),
        )
        .unwrap(),
        Tag::BytesObject => RawVal::try_from_val(host, &vec![1]).unwrap(),
        Tag::StringObject => RawVal::try_from_val(host, &"foo").unwrap(),
        Tag::SymbolObject => RawVal::try_from_val(
            host,
            &ScVal::Symbol(xdr::ScSymbol::try_from("a-big-symbol").unwrap()),
        )
        .unwrap(),
        Tag::VecObject => {
            RawVal::try_from_val(host, &ScVal::Vec(Some(xdr::ScVec::try_from((0,)).unwrap())))
                .unwrap()
        }
        Tag::MapObject => RawVal::try_from_val(
            host,
            &ScVal::Map(Some(xdr::ScMap::try_from(vec![]).unwrap())),
        )
        .unwrap(),
        Tag::ContractExecutableObject => RawVal::try_from_val(
            host,
            &ScVal::ContractExecutable(xdr::ScContractExecutable::Token),
        )
        .unwrap(),
        Tag::AddressObject => RawVal::try_from_val(
            host,
            &ScVal::Address(xdr::ScAddress::Contract(xdr::Hash([0; 32]))),
        )
        .unwrap(),
        Tag::LedgerKeyNonceObject => panic!(),
        Tag::ObjectCodeUpperBound => panic!(),
        Tag::Bad => panic!(),
        // NB: do not add a fallthrough case here if new Tag variants are added.
        // this test depends on the match being exhaustive in order to ensure
        // the correctness of Tag discriminants.
    };

    assert_eq!(ex.get_tag(), tag);

    ex
}
