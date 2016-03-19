-- DiMem.hs
--
-- DSL for distributed memory computation - define and execute distributed memory programs
-- in type-safe manner.
--
-- Copyright (C) Serguey Zefirov, 2016

{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}

{-# OPTIONS -fno-warn-tabs #-}

module DiMem where

newtype Id a = Id Int
	deriving (Eq, Ord, Show)

data Rgn a where
	-- |Region has id, optional parent region,
	Rgn :: Id Rgn -> Maybe (Rgn a) -> Rgn a

data DM a where
	-- |Task has id, name, 
	Task :: Id "task" -> String -> DM ()

