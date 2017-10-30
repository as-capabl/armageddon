{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module
    AuthDB.Types
where

import AuthDB.Init

$(defineTable "~/.armageddon/auth.sqlite3" "file")
$(defineTable "~/.armageddon/auth.sqlite3" "config")
$(defineTable "~/.armageddon/auth.sqlite3" "host")
$(defineTable "~/.armageddon/auth.sqlite3" "registration")
