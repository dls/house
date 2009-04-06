-- THIS SHOULD BE RENAMED 

module Kernel.UserSpace (
   H,
   H.pageSize,

   H.PAddr, H.PhysPage, H.POffset,
   H.allocPhysPage, H.freePhysPage,
   getPAddr, setPAddr,

   H.VAddr, H.minVAddr, H.maxVAddr,
   H.PageInfo (..), H.PageMap, setPage, getPage, allocPageMap, freePageMap,

   H.Context (..), H.Interrupt (..), H.ErrorCode, H.PageFaultErrorCode (..),
   execContext, {- H.zeroContext, -}

   registerIRQHandler, callIRQHandler
)
where

import Kernel.HInterface
import H.All(H)
import qualified H.All as H
import qualified Kernel.Interrupts as K(registerIRQHandler,callIRQHandler) -- temporary (?)

instance HPhysMemMonad H where
  getPAddr = H.getPAddr
  setPAddr = H.setPAddr

instance HPageMonad H where
  getPage = H.getPage
  setPage = H.setPage

instance HPageMapMonad H where
  allocPageMap = H.allocPageMap
  freePageMap = H.freePageMap

instance HExecMonad H where
  execContext = H.execContext

instance HIOMonad H where
  registerIRQHandler = K.registerIRQHandler
  callIRQHandler     = K.callIRQHandler
