use std::ops::Range;

use soroban_env_common::xdr::{ScErrorCode, ScErrorType};
use soroban_env_common::U32Val;

use crate::events::{CONTRACT_EVENT_TOPICS_LIMIT, TOPIC_BYTES_LENGTH_LIMIT};
use crate::{host_object::HostObject, Host, HostError, Object, Val, VecObject};

impl Host {
    // Notes on metering: free
    pub(crate) fn validate_index_lt_bound(
        &self,
        index: u32,
        bound: usize,
    ) -> Result<(), HostError> {
        if index as usize >= bound {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "index out of bound",
                &[U32Val::from(index).to_val()],
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
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "index out of bound",
                &[U32Val::from(index).to_val()],
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
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "start index out of bound",
                &[U32Val::from(start).to_val()],
            ));
        }
        if end as usize > bound {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "end index out of bound",
                &[U32Val::from(end).to_val()],
            ));
        }
        if start > end {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::InvalidInput,
                "start index greater than end index",
                &[U32Val::from(start).to_val(), U32Val::from(end).to_val()],
            ));
        }
        Ok(Range {
            start: start as usize,
            end: end as usize,
        })
    }

    // Metering: covered by components
    // TODO: the validation is incomplete. Need to further restrict Map, Vec sizes.
    fn validate_topic(&self, topic: Val) -> Result<(), HostError> {
        if let Ok(topic) = Object::try_from(topic) {
            unsafe {
                self.unchecked_visit_val_obj(topic, |ob| {
                    match ob {
                        None => Err(self.err(
                            ScErrorType::Object,
                            ScErrorCode::MissingValue,
                            "topic is an unknown object",
                            &[],
                        )),
                        Some(ho) => match ho {
                            HostObject::Bytes(b) => {
                                if b.len() > TOPIC_BYTES_LENGTH_LIMIT {
                                    // TODO: use more event-specific error codes than `UnexpectedType`.
                                    // Something like "topic bytes exceeds length limit"
                                    Err(self.err(
                                        ScErrorType::Object,
                                        ScErrorCode::ExceededLimit,
                                        "topic exceeds length limit",
                                        &[topic.to_val()],
                                    ))
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
                    None => Err(self.err(
                        ScErrorType::Object,
                        ScErrorCode::MissingValue,
                        "topic is an unknown object",
                        &[],
                    )),
                    Some(ho) => match ho {
                        HostObject::Vec(vv) => {
                            if vv.len() > CONTRACT_EVENT_TOPICS_LIMIT {
                                // TODO: proper error code "event topics exceeds count limit"
                                return Err(self.err(
                                    ScErrorType::Object,
                                    ScErrorCode::ExceededLimit,
                                    "topic vector exceeds length limit",
                                    &[topics.to_val()],
                                ));
                            }
                            for &topic in vv.iter() {
                                self.validate_topic(topic)?;
                            }
                            Ok(())
                        }
                        _ => Err(self.err(
                            ScErrorType::Object,
                            ScErrorCode::UnexpectedType,
                            "topics-vector was unexpected type",
                            &[topics.to_val()],
                        )),
                    },
                }
            })
        }
    }
}
