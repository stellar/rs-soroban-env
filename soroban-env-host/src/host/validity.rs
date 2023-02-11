use std::ops::Range;

use soroban_env_common::VecObject;

use crate::events::{DebugError, CONTRACT_EVENT_TOPICS_LIMIT, TOPIC_BYTES_LENGTH_LIMIT};
use crate::xdr::{ScHostFnErrorCode, ScHostObjErrorCode};
use crate::{host_object::HostObject, Host, HostError, Object, RawVal};

impl Host {
    // Notes on metering: free
    pub(crate) fn validate_index_lt_bound(
        &self,
        index: u32,
        bound: usize,
    ) -> Result<(), HostError> {
        if index as usize >= bound {
            return Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound, // TODO: need to reconcile between InputArgsInvalid and VecIndexOutOfBound
                "start index out of bound",
            ));
        }
        Ok(())
    }

    // Notes on metering: free
    pub(crate) fn validate_index_le_bound(
        &self,
        index: u32,
        bound: usize,
    ) -> Result<(), HostError> {
        if index as usize > bound {
            return Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound, // TODO: need to reconcile between InputArgsInvalid and VecIndexOutOfBound
                "start index out of bound",
            ));
        }
        Ok(())
    }

    // Notes on metering: free
    pub(crate) fn valid_range_from_start_end_bound(
        &self,
        start: u32,
        end: u32,
        bound: usize,
    ) -> Result<Range<usize>, HostError> {
        if start as usize > bound {
            return Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound,
                "start index out of bound",
            ));
        }
        if end as usize > bound {
            return Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound,
                "end index out of bound",
            ));
        }
        if start > end {
            return Err(self.err(
                DebugError::new(ScHostFnErrorCode::InputArgsInvalid)
                    .msg("index starts at {} but ends at {}")
                    .arg(RawVal::from_u32(start).to_raw())
                    .arg(RawVal::from_u32(end).to_raw()),
            ));
        }
        Ok(Range {
            start: start as usize,
            end: end as usize,
        })
    }

    // Notes on metering: free
    pub(crate) fn valid_range_from_start_span_bound(
        &self,
        start: u32,
        span: u32,
        bound: usize,
    ) -> Result<Range<usize>, HostError> {
        let end = start.checked_add(span).ok_or_else(|| {
            self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
        })?;
        self.valid_range_from_start_end_bound(start, end, bound)
    }

    // Metering: covered by components
    // TODO: the validation is incomplete. Need to further restrict Map, Vec sizes.
    fn validate_topic(&self, topic: RawVal) -> Result<(), HostError> {
        if let Ok(topic) = Object::try_from(topic) {
            unsafe {
                self.unchecked_visit_val_obj(topic, |ob| {
                    match ob {
                        None => Err(self.err_status(ScHostObjErrorCode::UnknownReference)),
                        Some(ho) => match ho {
                            HostObject::ContractExecutable(_) => {
                                Err(self.err_status(ScHostObjErrorCode::UnexpectedType))
                            }
                            HostObject::Bytes(b) => {
                                if b.len() > TOPIC_BYTES_LENGTH_LIMIT {
                                    // TODO: use more event-specific error codes than `UnexpectedType`.
                                    // Something like "topic bytes exceeds length limit"
                                    Err(self.err_status(ScHostObjErrorCode::UnexpectedType))
                                } else {
                                    Ok(())
                                }
                            }
                            _ => Ok(()),
                        },
                    }
                })
            }
        } else {
            Ok(())
        }
    }

    pub(crate) fn validate_contract_event_topics(
        &self,
        topics: VecObject,
    ) -> Result<(), HostError> {
        unsafe {
            self.unchecked_visit_val_obj(topics, |ob| {
                match ob {
                    None => Err(self.err_status(ScHostObjErrorCode::UnknownReference)),
                    Some(ho) => match ho {
                        HostObject::Vec(vv) => {
                            if vv.len() > CONTRACT_EVENT_TOPICS_LIMIT {
                                // TODO: proper error code "event topics exceeds count limit"
                                return Err(self.err_status(ScHostObjErrorCode::UnknownError));
                            }
                            for &topic in vv.iter() {
                                self.validate_topic(topic)?;
                            }
                            Ok(())
                        }
                        _ => Err(self.err_status(ScHostObjErrorCode::UnexpectedType)),
                    },
                }
            })
        }
    }
}
