{-# LANGUAGE
	TemplateHaskell,
	TypeOperators #-}
module Object.Templates(
	makeName,
	makeObject
	) where

import Object.Letters
import Object.Types

import Prelude hiding ((.))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char
import Data.Maybe

-- |
-- takes lower case 'foo' and makes
-- 'type Foo = Method (T_f,T_o,T_o)'
-- 'foo = Method (T_f,T_o,T_o) :: Foo'
makeName :: String -> Q [Dec]
makeName name = fst `fmap` makeName' name

makeName' :: String -> Q ([Dec],(Name,Name))
makeName' name = go where
	go
		| [] <- name = fail "can't make empty variable"
		| not $ isLower $ head name = fail $ name ++ ": does not start with a lower letter"
		| (first:rest) <- name = do
			typeTuple <- mapM typeCon name
			dataTuple <- mapM dataCon name
			typeName <- newName $ [toUpper first] ++ rest ++ ['_']
			dataName <- newName $ [first] ++ rest
			typ <- [t| Method $(return $ foldl AppT (TupleT (length typeTuple)) typeTuple) |]
			dat <- [| Method $(return $ (TupE dataTuple)) :: $(return $ ConT typeName) |]
			let typeDecl = TySynD typeName [] typ
			let dataDecl = ValD (VarP dataName) (NormalB dat) []
			return ([typeDecl,dataDecl],(typeName,dataName))
	typeCon c = do
		Just res <- lookupTypeName $ "T_" ++ [c] 
		return $ ConT res
	dataCon c = do
		Just res <- lookupValueName $ "T_" ++ [c] 
		return $ ConE res

-- |
-- returns (typeName, variableNames, and fields)
getInfo :: Info -> Q (Name, [Name], [VarStrictType])
getInfo (TyConI (DataD context typeName vars [RecC constrName fields] _)) = go where
	go = return (typeName, map getVar vars, fields)
	getVar (PlainTV n) = n
	getVar (KindedTV n _) = n
getInfo _ = fail $ "type needs to have a single constructor record type"

getFieldName (fieldName,strictness,type')
	| nameBase fieldName !! 0 /= '_' || not (isLower $ nameBase fieldName !! 1)
		= fail $ show fieldName ++
			": all fieldNames must commence with a '_' \
			\and continue with a lower case letter"
	| otherwise = nameBase fieldName

-- |
-- takes a Type with one record constructor
-- 'setGetFunctional \'\'Foo'
-- and produces
-- set and get instances for all fields
makeObject :: Name -> Q [Dec]
makeObject name = go name where
	go :: Name -> Q [Dec]
	go obj = do
		(name, vars, fields) <- reify name >>= getInfo
		let objType = foldl AppT (ConT name) (VarT `fmap` vars)
		concat `fmap` (sequence $ makeField name vars `fmap` fields)
-- "(Object.Example.Foo,[x_1627454179],[(Object.Example._bar,NotStrict,ConT GHC.Types.Int),(Object.Example._baz,NotStrict,ConT GHC.Types.Char),(Object.Example._blub,NotStrict,VarT x_1627454179)])"
	makeField ::  Name -> [Name] -> VarStrictType -> Q [Dec]
	makeField _ _ (name,_,_) | '_' /= head (nameBase name) = fail $ show name ++ " did not start with underscore"
	makeField name vars (fName, _, fType) = do
		(decs1,(typeName,dataName)) <- makeName' (tail $ nameBase fName)
		let objType = foldl AppT (ConT name) (VarT `fmap` vars)

		actionInst <- [d|
			instance (value ~ $(return fType)) => Action $(return objType) $(return $ ConT typeName) value where
				object . _ = $(return $ VarE fName) object
			|]

		matchType <- [t| $(return $ ConT typeName) := $(return $ VarT $ mkName "value") |]
		actionSetInst <- [d|
			instance (value ~ $(return fType), object ~ $(return objType)) => Action $(return objType) $(return matchType) object where
				object . ( _ := v) = $(recUpdE [e|object|] [return (fName, VarE $ mkName "v")])
			|]

		return $ actionInst ++ actionSetInst ++ decs1
