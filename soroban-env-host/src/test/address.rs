use crate::Host;
use soroban_env_common::{
    xdr::{AccountId, Hash, PublicKey, ScAddress, Uint256},
    Env, TryIntoVal,
};

#[test]
fn test_account_address_conversions() {
    let host = Host::default();
    let account_pk = [5_u8; 32];
    let account_pk_obj = host.add_host_object(account_pk.to_vec()).unwrap();
    let address_obj = host.account_public_key_to_address(account_pk_obj).unwrap();
    assert_eq!(
        host.visit_obj(address_obj, |addr: &ScAddress| { Ok(addr.clone()) })
            .unwrap(),
        ScAddress::Account(AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
            account_pk.clone()
        ))))
    );
    let restored_pk_obj = host
        .address_to_account_public_key(address_obj)
        .unwrap()
        .try_into_val(&host)
        .unwrap();
    assert_eq!(
        host.visit_obj(restored_pk_obj, |pk: &Vec<u8>| Ok(pk.clone()))
            .unwrap(),
        account_pk.to_vec()
    );
    // Verify that the trying to get the contract id returns the unit type.
    // As RawVals aren't comparable, we use `try_into_val` to do the verification
    // instead.
    let _: () = host
        .address_to_contract_id(address_obj)
        .unwrap()
        .try_into_val(&host)
        .unwrap();
}

#[test]
fn test_contract_address_conversions() {
    let host = Host::default();
    let contract_id = [222_u8; 32];
    let contract_id_obj = host.add_host_object(contract_id.to_vec()).unwrap();
    let address_obj = host.contract_id_to_address(contract_id_obj).unwrap();
    assert_eq!(
        host.visit_obj(address_obj, |addr: &ScAddress| { Ok(addr.clone()) })
            .unwrap(),
        ScAddress::Contract(Hash(contract_id.clone()))
    );
    let restored_contract_id_obj = host
        .address_to_contract_id(address_obj)
        .unwrap()
        .try_into_val(&host)
        .unwrap();
    assert_eq!(
        host.visit_obj(restored_contract_id_obj, |pk: &Vec<u8>| Ok(pk.clone()))
            .unwrap(),
        contract_id.to_vec()
    );
    // Verify that the trying to get the account key returns the unit type.
    // As RawVals aren't comparable, we use `try_into_val` to do the verification
    // instead.
    let _: () = host
        .address_to_account_public_key(address_obj)
        .unwrap()
        .try_into_val(&host)
        .unwrap();
}
