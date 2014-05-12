{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, RankNTypes, OverlappingInstances, NoMonomorphismRestriction, ExistentialQuantification #-}
module Object.Templates where

import Object.Letters
import Object.Types

import Prelude hiding ((.))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char

-- |
-- takes lower case 'foo' and makes
-- 'type Foo = (T_f,T_o,T_o)'
-- 'foo = (T_f,T_o,T_o) :: Foo'
makeMethodVariable :: String -> Q [Dec]
makeMethodVariable name = go where
	go
		| [] <- name = fail "can't make empty variable"
		| not $ isLower $ head name = fail $ name ++ ": does not start with a lower letter"
		| (first:rest) <- name = do
			typeTuple <- mapM typeCon name
			dataTuple <- mapM dataCon name
			typeName <- newName $ [toUpper first] ++ rest
			dataName <- newName $ [first] ++ rest
			let typeDecl = TySynD typeName [] (foldl AppT (TupleT (length typeTuple)) typeTuple)
			let dataDecl = ValD (VarP dataName) (NormalB (SigE (TupE dataTuple) (ConT typeName))) []
			return [typeDecl,dataDecl]
	typeCon c = do
		Just res <- lookupTypeName $ "T_" ++ [c] 
		return $ ConT res
	dataCon c = do
		Just res <- lookupValueName $ "T_" ++ [c] 
		return $ ConE res

-- restrictAssociateObjectList :: Name -> Q [Dec]
-- restrictAssociateObjectList name = go $ return $ ConT name where
-- 	go obj = 
-- 		[d|
-- 			type instance Restrict $obj [AssociateObject foo method] = [AssociateObject $obj method]
-- 			type instance Restrict $obj (AssociateObject foo method) = AssociateObject $obj method
-- 		|]

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
setGetFunctional :: Name -> Q [Dec]
setGetFunctional name = go name where
	go :: Name -> Q [Dec]
	go obj = do
		info <- reify name >>= getInfo
		let tt = return $ LitE $ StringL $ show info
		[d|
			a = $([| $tt |])
			|]
