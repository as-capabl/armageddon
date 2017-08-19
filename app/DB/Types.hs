{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module
    DB.Types
where

import DB.Init


$(defineTable "~/.armageddon/auth.sqlite3" "registration")
