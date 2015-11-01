module QuickLift.State where

import BigPrelude

import Optic.Lens
import Optic.Core

import Types
import Types.Date

import QuickLift.Model

type State =
  { currentPage :: Routes
  , currentUser :: Maybe User
  , loadedSessions :: Array Session
  , currentSession :: Session
  , registration :: UserReg
  , authentication :: UserAuth
  }

initialState :: State
initialState =
  { currentPage: Home
  , currentUser: Nothing
  , loadedSessions: []
  , currentSession: emptySession
  , registration: emptyReg
  , authentication: emptyAuth
  }

stRegistration :: LensP State UserReg
stRegistration =
  lens
    (_.registration)
    (_ { registration = _ })

stCurrentSession :: LensP State Session
stCurrentSession =
  lens
    (_.currentSession)
    (_ { currentSession = _ })

stLoadedSessions :: LensP State (Array Session)
stLoadedSessions =
  lens 
    (_.loadedSessions)
    (_ { loadedSessions = _ })

stCurrentUser :: LensP State (Maybe User)
stCurrentUser =
  lens
    (_.currentUser)
    (_ { currentUser = _ })

stAuthentication :: LensP State UserAuth
stAuthentication = lens _.authentication _ { authentication = _ }
