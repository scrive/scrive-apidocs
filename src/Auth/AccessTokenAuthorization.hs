module Auth.AccessTokenAuthorization where

-- | API Access Token
data AccessTokenAuthorization = AccessTokenAuthorization MinutesTime UserID [Privilege]

instance DocumentAuthorization AccessTokenAuthorization where
  canDocument (AccessTokenAuthorization ex _ _ ) _ now _ | ex <= now                        = False
  canDocument (AccessTokenAuthorization _ _ ps ) _ _ p   | DocumentPrivilege p `notElem` ps = False
  canDocument (AccessTokenAuthorization _ uid _) doc now p =
    canDocument (AuthenticatedUserAuthorization uid) doc now p 

instance UserAuthorization AccessTokenAuthorization where
  canUser (AccessTokenAuthorization ex _ _  ) _ now _  | ex <= now                    = False
  canUser (AccessTokenAuthorization _ _ ps  ) _ _ p    | UserPrivilege p `notElem` ps = False
  canUser (AccessTokenAuthorization _ uid _ ) uid2 _ _ | uid /= uid2                  = False
  canUser (AccessTokenAuthorization _ uid _ ) uid2 now p =
    canUser (AuthenticatedUserAuthorization uid) uid2 now p
