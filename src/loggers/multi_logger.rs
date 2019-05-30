use crate::log::{Record, Level, Metadata};

pub struct MultiLogger(Vec<Option<Box<log::Log>>>);

impl log::Log for MultiLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        self.0.iter().any(|ol| match ol {
            Some(l) => l.enabled(metadata),
            None => false,
        })
    }

    fn log(&self, record: &Record) {
        for l in &self.0 {
            match l {
                Some(l) => {
                    if l.enabled(record.metadata()) {
                        l.log(record);
                    }
                },
                None => (),
            }
        }
    }

    fn flush(&self) {
        for l in &self.0 {
            match l {
                Some(l) => l.flush(),
                None => (),
            }
        }
    }
}
