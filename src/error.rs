use backtrace::Backtrace;
use crate::r2d2;

#[derive(Debug)]
pub struct CetrizineError{
    pub error_type: CetrizineErrorType,
    pub backtrace: Backtrace,
}

impl CetrizineError {
    fn new(error_type: CetrizineErrorType) -> Self {
        CetrizineError{
            error_type,
            backtrace: Backtrace::new(),
        }
    }
}

macro_rules! error_type {
    ( $enum_ty:ident, $struct_ty:ident, {
        $( $variant_name:ident ( $variant_ty:ty ) , )+
    } ) => {
        #[derive(Debug)]
        pub enum $enum_ty {
            $( $variant_name($variant_ty), )+
        }

        impl ::std::error::Error for $struct_ty {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                match &self.error_type {
                    $( $enum_ty::$variant_name(e) => Some(e), )+
                }
            }
        }

        $(
            impl From<$variant_ty> for $struct_ty {
                fn from(err: $variant_ty) -> Self {
                    $struct_ty::new($enum_ty::$variant_name(err))
                }
            }
        )+
    };
}

error_type! {
    CetrizineErrorType, CetrizineError, {
        Pool(r2d2::PoolError),
        Sql(diesel::result::Error),
        Serenity(serenity::Error),
        Io(std::io::Error),
        SerdeJson(serde_json::Error),
    }
}

impl std::fmt::Display for CetrizineError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
