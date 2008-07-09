;;;; x-repo-rep-meta.lisp

;;; Syntax: (class-name kinds representation-form)
;;;  class-name = CORBA class name
;;;  kinds = list of definition kind constant symbols (or t for default?)
;;;  representation-form:
;;;     list -> list elements processed recursively
;;;     symbol -> literal symbol or 'contents -> list of recursive processed contents of container
;;;     string -> attribute of repo object
;;;     (> <string> typecode) -> typecode representation of attribute
;;;     (> <string> any-value) -> plain value of attribute
;;;     (@ <string> form) -> list of representations ..


;;(CORBA:ValueMemberDef (:dk_ValueMember) (valuemember ))

(CORBA:Repository (:dk_Repository) (repository () . contents))

(CORBA:ModuleDef (:dk_Module) (module "absolute_name" . contents))

(CORBA:InterfaceDef (:dk_Interface :dk_AbstractInterface :dk_LocalInterface)
 (interface "absolute_name"
            (@ "base_interfaces" "id")
            (> "type" typecode) 
            . contents))

(CORBA:StructDef (:dk_Struct) (struct "absolute_name" (> "type" typecode) . contents))

(CORBA:ExceptionDef (:dk_Exception) (exception "absolute_name" (> "type" typecode) . contents))

(CORBA:UnionDef (:dk_Union) (union "absolute_name" (> "type" typecode) . contents))

(CORBA:AttributeDef (:dk_Attribute) (attribute "name" "mode" (> "type" typecode)))

(CORBA:OperationDef (:dk_Operation)
 (operation "name" "mode"
            (> "result" typecode)
            (@ "params" ("name" "mode" (> "type" typecode)))
            (@ "exceptions" "id")))

(CORBA:ConstantDef (:dk_Constant) ;;unless (clorb::enum-constant-p def)
                   (const "absolute_name" (> "type" typecode) (> "value" any-value)))

(CORBA:IDLType (:dk_Typedef :dk_Enum :dk_Alias :dk_String :dk_Sequence :dk_Array
                            :dk_Wstring :dk_Fixed :dk_Primitive :dk_Fixed :dk_Native
                            :dk_ValueBox :dk_Value)
               (type "absolute_name" (> "type" typecode)))

(CORBA:Contained t (misc "def_kind" "absolute_name"))


;;;   enum DefinitionKind {
;;;     dk_none, dk_all, dk_Attribute, dk_Constant, dk_Exception,
;;;     dk_Interface, dk_Module, dk_Operation, dk_Typedef, dk_Alias,
;;;     dk_Struct, dk_Union, dk_Enum, dk_Primitive, dk_String,
;;;     dk_Sequence, dk_Array, dk_Repository, dk_Wstring, dk_Fixed,
;;;     dk_Value, dk_ValueBox, dk_ValueMember,
;;;     dk_Native, dk_AbstractInterface, dk_LocalInterface
;;;   };
