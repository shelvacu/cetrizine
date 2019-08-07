use backtrace::Backtrace;
use r2d2_postgres::r2d2;

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
        Pool(r2d2::Error),
        Sql(diesel::result::Error),
        Serenity(serenity::Error),
        Io(std::io::Error),
        SerdeJson(serde_json::Error),
    }
}

/*#[derive(Debug)]
pub enum CetrizineErrorType{
    Pool(r2d2::Error),
    Sql(diesel::result::Error),
    Serenity(serenity::Error),
    Io(std::io::Error),
}*/

impl std::fmt::Display for CetrizineError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/*impl std::error::Error for CetrizineError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use CetrizineErrorType::*;
        match &self.error_type {
            Pool(e)     => Some(e),
            Sql(e)      => Some(e),
            Serenity(e) => Some(e),
            Io(e)       => Some(e),
        }
    }
}

impl From<diesel::result::Error> for CetrizineError {
    fn from(err: diesel::result::Error) -> Self {
        CetrizineError::new(CetrizineErrorType::Sql(err))
    }
}

impl From<serenity::Error> for CetrizineError {
    fn from(err: serenity::Error) -> Self {
        CetrizineError::new(CetrizineErrorType::Serenity(err))
    }
}

impl From<r2d2::Error> for CetrizineError {
    fn from(err: r2d2::Error) -> Self {
        CetrizineError::new(CetrizineErrorType::Pool(err))
    }
}

impl From<std::io::Error> for CetrizineError {
    fn from(err: std::io::Error) -> Self {
        CetrizineError::new(CetrizineErrorType::Io(err))
    }
}*/
