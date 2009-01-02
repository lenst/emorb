;;;; corba-meta.el -- Meta data about representations used

;; Copyright (C) 2007, 2008 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Syntax: (class-name kinds representation-form)
;;;  class-name = CORBA class name (not used)
;;;  kinds = list of definition kind constant symbols (or t for default?)
;;;  representation-form:
;;;     list -> list elements processed recursively
;;;     symbol -> literal symbol or 'contents -> list of recursive processed contents of container
;;;     string -> attribute of repo object
;;;     keyword -> field of struct
;;;     (> <string> typecode) -> typecode representation of attribute
;;;     (> <string> any-value) -> plain value of attribute
;;;     (@ <string> form) -> list of representations ..



(defconst corba-ifr-repr-meta '(
;;("ValueMemberDef" (:dk_ValueMember) (valuemember ))

("Repository" (:dk_Repository) (repository () . contents))

("ModuleDef" (:dk_Module) (module "absolute_name" . contents))

("InterfaceDef" (:dk_Interface :dk_AbstractInterface :dk_LocalInterface)
 (interface "absolute_name"
            (@ "base_interfaces" "id")
            (> "type" typecode) 
            . contents))

("StructDef" (:dk_Struct) (struct "absolute_name" (> "type" typecode) . contents))

("ExceptionDef" (:dk_Exception) (exception "absolute_name" (> "type" typecode) . contents))

("UnionDef" (:dk_Union) (union "absolute_name" (> "type" typecode) . contents))

("AttributeDef" (:dk_Attribute) (attribute "name" "mode" (> "type" typecode)))

("OperationDef" (:dk_Operation)
 (operation "name" "mode"
            (> "result" typecode)
            (@ "params" (:name :mode (> :type typecode)))
            (@ "exceptions" "id")))

("ConstantDef" (:dk_Constant) ;;unless (clorb::enum-constant-p def)
                   (const "absolute_name" (> "type" typecode) (> "value" any-value)))

("IDLType" (:dk_Typedef :dk_Enum :dk_Alias :dk_String :dk_Sequence :dk_Array
                            :dk_Wstring :dk_Fixed :dk_Primitive :dk_Fixed :dk_Native
                            :dk_ValueBox :dk_Value)
               (type "absolute_name" (> "type" typecode)))

("Contained" t (misc "def_kind" "absolute_name"))
))

;;;   enum DefinitionKind {
;;;     dk_none, dk_all, dk_Attribute, dk_Constant, dk_Exception,
;;;     dk_Interface, dk_Module, dk_Operation, dk_Typedef, dk_Alias,
;;;     dk_Struct, dk_Union, dk_Enum, dk_Primitive, dk_String,
;;;     dk_Sequence, dk_Array, dk_Repository, dk_Wstring, dk_Fixed,
;;;     dk_Value, dk_ValueBox, dk_ValueMember,
;;;     dk_Native, dk_AbstractInterface, dk_LocalInterface
;;;   };


(provide 'corba-meta)

;;; corba-meta.el ends here
