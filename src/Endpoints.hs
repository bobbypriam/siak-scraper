module Endpoints where

type Url = String

baseUrl :: Url
baseUrl = "https://academic.ui.ac.id/main"

authenticationUrl :: Url
authenticationUrl = baseUrl ++ "/Authentication/Index"

changeRoleUrl :: Url
changeRoleUrl = baseUrl ++ "/Authentication/ChangeRole"

summaryUrl :: Url
summaryUrl = baseUrl ++ "/Academic/Summary"
