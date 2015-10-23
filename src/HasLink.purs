module HasLink where

import Types

class HasLink a where
  link :: a -> String

instance routesHasLink :: HasLink Routes where
  link Profile = "#/profile"
  link (Sessions crud) = "#/sessions" ++ link crud
  link Home = "#/"

instance routesHasLink :: HasLink CRUD where
  link Index = ""
  link New = "/new"
  link (Show n) = "/" ++ show n
