;;; corba-load-ifr.el -- Load CORBA repository with interface repository types

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


(provide 'corba-load-ifr)

(corba-load-repository
 '(repository nil
 (module "::CORBA"
  (type "::CORBA::Identifier"
   (:tk_alias "IDL:omg.org/CORBA/Identifier:1.0" "Identifier" (:tk_string 0)))
  (type "::CORBA::ScopedName"
   (:tk_alias "IDL:omg.org/CORBA/ScopedName:1.0" "ScopedName" (:tk_string 0)))
  (type "::CORBA::RepositoryId"
   (:tk_alias "IDL:omg.org/CORBA/RepositoryId:1.0" "RepositoryId"
    (:tk_string 0)))
  (type "::CORBA::DefinitionKind"
   (:tk_enum "IDL:omg.org/CORBA/DefinitionKind:1.0" "DefinitionKind"
    ("dk_none" "dk_all" "dk_Attribute" "dk_Constant" "dk_Exception"
     "dk_Interface" "dk_Module" "dk_Operation" "dk_Typedef" "dk_Alias"
     "dk_Struct" "dk_Union" "dk_Enum" "dk_Primitive" "dk_String" "dk_Sequence"
     "dk_Array" "dk_Repository" "dk_Wstring" "dk_Fixed" "dk_Value"
     "dk_ValueBox" "dk_ValueMember" "dk_Native" "dk_AbstractInterface"
     "dk_LocalInterface")))
  (interface "::CORBA::IRObject" nil
   (:tk_objref "IDL:omg.org/CORBA/IRObject:1.0" "IRObject")
   (attribute "def_kind" :attr_readonly "IDL:omg.org/CORBA/DefinitionKind:1.0")
   (operation "destroy" :op_normal (:tk_void) nil nil))
  (type "::CORBA::VersionSpec"
   (:tk_alias "IDL:omg.org/CORBA/VersionSpec:1.0" "VersionSpec"
    (:tk_string 0)))
  (interface "::CORBA::Contained" ("IDL:omg.org/CORBA/IRObject:1.0")
   (:tk_objref "IDL:omg.org/CORBA/Contained:1.0" "Contained")
   (attribute "id" :attr_normal "IDL:omg.org/CORBA/RepositoryId:1.0")
   (attribute "name" :attr_normal "IDL:omg.org/CORBA/Identifier:1.0")
   (attribute "version" :attr_normal "IDL:omg.org/CORBA/VersionSpec:1.0")
   (attribute "defined_in" :attr_readonly
    (:tk_objref "IDL:omg.org/CORBA/Container:1.0" "Container"))
   (attribute "absolute_name" :attr_readonly
    "IDL:omg.org/CORBA/ScopedName:1.0")
   (attribute "containing_repository" :attr_readonly
    (:tk_objref "IDL:omg.org/CORBA/Repository:1.0" "Repository"))
   (struct "::CORBA::Contained::Description"
    (:tk_struct "IDL:omg.org/CORBA/Contained/Description:1.0" "Description"
     (("kind" "IDL:omg.org/CORBA/DefinitionKind:1.0") ("value" (:tk_any)))))
   (operation "describe" :op_normal
    "IDL:omg.org/CORBA/Contained/Description:1.0" nil nil)
   (operation "move" :op_normal (:tk_void)
    (("new_container" :param_in "IDL:omg.org/CORBA/Container:1.0")
     ("new_name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("new_version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0"))
    nil))
  (interface "::CORBA::Repository" ("IDL:omg.org/CORBA/Container:1.0")
   "IDL:omg.org/CORBA/Repository:1.0"
   (operation "lookup_id" :op_normal "IDL:omg.org/CORBA/Contained:1.0"
    (("search_id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")) nil)
   (operation "get_canonical_typecode" :op_normal (:tk_typecode)
    (("tc" :param_in (:tk_typecode))) nil)
   (operation "get_primitive" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/PrimitiveDef:1.0" "PrimitiveDef")
    (("kind" :param_in
      (:tk_enum "IDL:omg.org/CORBA/PrimitiveKind:1.0" "PrimitiveKind"
       ("pk_null" "pk_void" "pk_short" "pk_long" "pk_ushort" "pk_ulong"
        "pk_float" "pk_double" "pk_boolean" "pk_char" "pk_octet" "pk_any"
        "pk_TypeCode" "pk_Principal" "pk_string" "pk_objref" "pk_longlong"
        "pk_ulonglong" "pk_longdouble" "pk_wchar" "pk_wstring"
        "pk_value_base"))))
    nil)
   (operation "create_string" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/StringDef:1.0" "StringDef")
    (("bound" :param_in (:tk_ulong))) nil)
   (operation "create_wstring" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/WstringDef:1.0" "WstringDef")
    (("bound" :param_in (:tk_ulong))) nil)
   (operation "create_sequence" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/SequenceDef:1.0" "SequenceDef")
    (("bound" :param_in (:tk_ulong))
     ("element_type" :param_in
      (:tk_objref "IDL:omg.org/CORBA/IDLType:1.0" "IDLType")))
    nil)
   (operation "create_array" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/ArrayDef:1.0" "ArrayDef")
    (("length" :param_in (:tk_ulong))
     ("element_type" :param_in "IDL:omg.org/CORBA/IDLType:1.0"))
    nil)
   (operation "create_fixed" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/FixedDef:1.0" "FixedDef")
    (("digits" :param_in (:tk_ushort)) ("scale" :param_in (:tk_short))) nil))
  (interface "::CORBA::Container" ("IDL:omg.org/CORBA/IRObject:1.0")
   "IDL:omg.org/CORBA/Container:1.0"
   (operation "lookup" :op_normal "IDL:omg.org/CORBA/Contained:1.0"
    (("search_name" :param_in "IDL:omg.org/CORBA/ScopedName:1.0")) nil)
   (operation "contents" :op_normal
    (:tk_alias "IDL:omg.org/CORBA/ContainedSeq:1.0" "ContainedSeq"
     (:tk_sequence "IDL:omg.org/CORBA/Contained:1.0" 0))
    (("limit_type" :param_in "IDL:omg.org/CORBA/DefinitionKind:1.0")
     ("exclude_inherited" :param_in (:tk_boolean)))
    nil)
   (operation "lookup_name" :op_normal "IDL:omg.org/CORBA/ContainedSeq:1.0"
    (("search_name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("levels_to_search" :param_in (:tk_long))
     ("limit_type" :param_in "IDL:omg.org/CORBA/DefinitionKind:1.0")
     ("exclude_inherited" :param_in (:tk_boolean)))
    nil)
   (struct "::CORBA::Container::Description"
    (:tk_struct "IDL:omg.org/CORBA/Container/Description:1.0" "Description"
     (("contained_object" "IDL:omg.org/CORBA/Contained:1.0")
      ("kind" "IDL:omg.org/CORBA/DefinitionKind:1.0") ("value" (:tk_any)))))
   (type "::CORBA::Container::DescriptionSeq"
    (:tk_alias "IDL:omg.org/CORBA/Container/DescriptionSeq:1.0"
     "DescriptionSeq"
     (:tk_sequence "IDL:omg.org/CORBA/Container/Description:1.0" 0)))
   (operation "describe_contents" :op_normal
    "IDL:omg.org/CORBA/Container/DescriptionSeq:1.0"
    (("limit_type" :param_in "IDL:omg.org/CORBA/DefinitionKind:1.0")
     ("exclude_inherited" :param_in (:tk_boolean))
     ("max_returned_objs" :param_in (:tk_long)))
    nil)
   (operation "create_module" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/ModuleDef:1.0" "ModuleDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0"))
    nil)
   (operation "create_constant" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/ConstantDef:1.0" "ConstantDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("type" :param_in "IDL:omg.org/CORBA/IDLType:1.0")
     ("value" :param_in (:tk_any)))
    nil)
   (operation "create_struct" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/StructDef:1.0" "StructDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("members" :param_in
      (:tk_alias "IDL:omg.org/CORBA/StructMemberSeq:1.0" "StructMemberSeq"
       (:tk_sequence
        (:tk_struct "IDL:omg.org/CORBA/StructMember:1.0" "StructMember"
         (("name" "IDL:omg.org/CORBA/Identifier:1.0") ("type" (:tk_typecode))
          ("type_def" "IDL:omg.org/CORBA/IDLType:1.0")))
        0))))
    nil)
   (operation "create_union" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/UnionDef:1.0" "UnionDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("discriminator_type" :param_in "IDL:omg.org/CORBA/IDLType:1.0")
     ("members" :param_in
      (:tk_alias "IDL:omg.org/CORBA/UnionMemberSeq:1.0" "UnionMemberSeq"
       (:tk_sequence
        (:tk_struct "IDL:omg.org/CORBA/UnionMember:1.0" "UnionMember"
         (("name" "IDL:omg.org/CORBA/Identifier:1.0") ("label" (:tk_any))
          ("type" (:tk_typecode))
          ("type_def" "IDL:omg.org/CORBA/IDLType:1.0")))
        0))))
    nil)
   (operation "create_enum" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/EnumDef:1.0" "EnumDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("members" :param_in
      (:tk_alias "IDL:omg.org/CORBA/EnumMemberSeq:1.0" "EnumMemberSeq"
       (:tk_sequence "IDL:omg.org/CORBA/Identifier:1.0" 0))))
    nil)
   (operation "create_alias" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/AliasDef:1.0" "AliasDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("original_type" :param_in "IDL:omg.org/CORBA/IDLType:1.0"))
    nil)
   (operation "create_interface" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/InterfaceDef:1.0" "InterfaceDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("base_interfaces" :param_in
      (:tk_alias "IDL:omg.org/CORBA/InterfaceDefSeq:1.0" "InterfaceDefSeq"
       (:tk_sequence "IDL:omg.org/CORBA/InterfaceDef:1.0" 0))))
    nil)
   (operation "create_value" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/ValueDef:1.0" "ValueDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("is_custom" :param_in (:tk_boolean))
     ("is_abstract" :param_in (:tk_boolean))
     ("base_value" :param_in "IDL:omg.org/CORBA/ValueDef:1.0")
     ("is_truncatable" :param_in (:tk_boolean))
     ("abstract_base_values" :param_in
      (:tk_alias "IDL:omg.org/CORBA/ValueDefSeq:1.0" "ValueDefSeq"
       (:tk_sequence "IDL:omg.org/CORBA/ValueDef:1.0" 0)))
     ("supported_interfaces" :param_in "IDL:omg.org/CORBA/InterfaceDefSeq:1.0")
     ("initializers" :param_in
      (:tk_alias "IDL:omg.org/CORBA/InitializerSeq:1.0" "InitializerSeq"
       (:tk_sequence
        (:tk_struct "IDL:omg.org/CORBA/Initializer:1.0" "Initializer"
         (("members" "IDL:omg.org/CORBA/StructMemberSeq:1.0")
          ("name" "IDL:omg.org/CORBA/Identifier:1.0")))
        0))))
    nil)
   (operation "create_value_box" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/ValueBoxDef:1.0" "ValueBoxDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("original_type_def" :param_in "IDL:omg.org/CORBA/IDLType:1.0"))
    nil)
   (operation "create_exception" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/ExceptionDef:1.0" "ExceptionDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("members" :param_in "IDL:omg.org/CORBA/StructMemberSeq:1.0"))
    nil)
   (operation "create_native" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/NativeDef:1.0" "NativeDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0"))
    nil)
   (operation "create_abstract_interface" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/AbstractInterfaceDef:1.0"
     "AbstractInterfaceDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("base_interfaces" :param_in
      (:tk_alias "IDL:omg.org/CORBA/AbstractInterfaceDefSeq:1.0"
       "AbstractInterfaceDefSeq"
       (:tk_sequence "IDL:omg.org/CORBA/AbstractInterfaceDef:1.0" 0))))
    nil)
   (operation "create_local_interface" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/LocalInterfaceDef:1.0" "LocalInterfaceDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("base_interfaces" :param_in "IDL:omg.org/CORBA/InterfaceDefSeq:1.0"))
    nil))
  (interface "::CORBA::ModuleDef"
   ("IDL:omg.org/CORBA/Container:1.0" "IDL:omg.org/CORBA/Contained:1.0")
   "IDL:omg.org/CORBA/ModuleDef:1.0")
  (interface "::CORBA::ConstantDef" ("IDL:omg.org/CORBA/Contained:1.0")
   "IDL:omg.org/CORBA/ConstantDef:1.0"
   (attribute "type" :attr_readonly (:tk_typecode))
   (attribute "type_def" :attr_normal "IDL:omg.org/CORBA/IDLType:1.0")
   (attribute "value" :attr_normal (:tk_any)))
  (interface "::CORBA::IDLType" ("IDL:omg.org/CORBA/IRObject:1.0")
   "IDL:omg.org/CORBA/IDLType:1.0"
   (attribute "type" :attr_readonly (:tk_typecode)))
  (interface "::CORBA::StructDef"
   ("IDL:omg.org/CORBA/TypedefDef:1.0" "IDL:omg.org/CORBA/Container:1.0")
   "IDL:omg.org/CORBA/StructDef:1.0"
   (attribute "members" :attr_normal "IDL:omg.org/CORBA/StructMemberSeq:1.0"))
  (interface "::CORBA::UnionDef"
   ("IDL:omg.org/CORBA/TypedefDef:1.0" "IDL:omg.org/CORBA/Container:1.0")
   "IDL:omg.org/CORBA/UnionDef:1.0"
   (attribute "discriminator_type" :attr_readonly (:tk_typecode))
   (attribute "discriminator_type_def" :attr_normal
    "IDL:omg.org/CORBA/IDLType:1.0")
   (attribute "members" :attr_normal "IDL:omg.org/CORBA/UnionMemberSeq:1.0"))
  (interface "::CORBA::EnumDef" ("IDL:omg.org/CORBA/TypedefDef:1.0")
   "IDL:omg.org/CORBA/EnumDef:1.0"
   (attribute "members" :attr_normal "IDL:omg.org/CORBA/EnumMemberSeq:1.0"))
  (interface "::CORBA::AliasDef" ("IDL:omg.org/CORBA/TypedefDef:1.0")
   "IDL:omg.org/CORBA/AliasDef:1.0"
   (attribute "original_type_def" :attr_normal
    "IDL:omg.org/CORBA/IDLType:1.0"))
  (interface "::CORBA::InterfaceDef"
   ("IDL:omg.org/CORBA/Container:1.0" "IDL:omg.org/CORBA/Contained:1.0"
    "IDL:omg.org/CORBA/IDLType:1.0")
   "IDL:omg.org/CORBA/InterfaceDef:1.0"
   (attribute "base_interfaces" :attr_normal
    "IDL:omg.org/CORBA/InterfaceDefSeq:1.0")
   (operation "is_a" :op_normal (:tk_boolean)
    (("interface_id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")) nil)
   (struct "::CORBA::InterfaceDef::FullInterfaceDescription"
    (:tk_struct "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0"
     "FullInterfaceDescription"
     (("name" "IDL:omg.org/CORBA/Identifier:1.0")
      ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
      ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
      ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
      ("operations"
       (:tk_alias "IDL:omg.org/CORBA/OpDescriptionSeq:1.0" "OpDescriptionSeq"
        (:tk_sequence
         (:tk_struct "IDL:omg.org/CORBA/OperationDescription:1.0"
          "OperationDescription"
          (("name" "IDL:omg.org/CORBA/Identifier:1.0")
           ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
           ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
           ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
           ("result" (:tk_typecode))
           ("mode"
            (:tk_enum "IDL:omg.org/CORBA/OperationMode:1.0" "OperationMode"
             ("OP_NORMAL" "OP_ONEWAY")))
           ("contexts"
            (:tk_alias "IDL:omg.org/CORBA/ContextIdSeq:1.0" "ContextIdSeq"
             (:tk_sequence
              (:tk_alias "IDL:omg.org/CORBA/ContextIdentifier:1.0"
               "ContextIdentifier" "IDL:omg.org/CORBA/Identifier:1.0")
              0)))
           ("parameters"
            (:tk_alias "IDL:omg.org/CORBA/ParDescriptionSeq:1.0"
             "ParDescriptionSeq"
             (:tk_sequence
              (:tk_struct "IDL:omg.org/CORBA/ParameterDescription:1.0"
               "ParameterDescription"
               (("name" "IDL:omg.org/CORBA/Identifier:1.0")
                ("type" (:tk_typecode))
                ("type_def" "IDL:omg.org/CORBA/IDLType:1.0")
                ("mode"
                 (:tk_enum "IDL:omg.org/CORBA/ParameterMode:1.0"
                  "ParameterMode" ("PARAM_IN" "PARAM_OUT" "PARAM_INOUT")))))
              0)))
           ("exceptions"
            (:tk_alias "IDL:omg.org/CORBA/ExcDescriptionSeq:1.0"
             "ExcDescriptionSeq"
             (:tk_sequence
              (:tk_struct "IDL:omg.org/CORBA/ExceptionDescription:1.0"
               "ExceptionDescription"
               (("name" "IDL:omg.org/CORBA/Identifier:1.0")
                ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
                ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
                ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
                ("type" (:tk_typecode))))
              0)))))
         0)))
      ("attributes"
       (:tk_alias "IDL:omg.org/CORBA/AttrDescriptionSeq:1.0"
        "AttrDescriptionSeq"
        (:tk_sequence
         (:tk_struct "IDL:omg.org/CORBA/AttributeDescription:1.0"
          "AttributeDescription"
          (("name" "IDL:omg.org/CORBA/Identifier:1.0")
           ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
           ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
           ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
           ("type" (:tk_typecode))
           ("mode"
            (:tk_enum "IDL:omg.org/CORBA/AttributeMode:1.0" "AttributeMode"
             ("ATTR_NORMAL" "ATTR_READONLY")))))
         0)))
      ("base_interfaces"
       (:tk_alias "IDL:omg.org/CORBA/RepositoryIdSeq:1.0" "RepositoryIdSeq"
        (:tk_sequence "IDL:omg.org/CORBA/RepositoryId:1.0" 0)))
      ("type" (:tk_typecode)))))
   (operation "describe_interface" :op_normal
    "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0" nil nil)
   (operation "create_attribute" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/AttributeDef:1.0" "AttributeDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("type" :param_in "IDL:omg.org/CORBA/IDLType:1.0")
     ("mode" :param_in "IDL:omg.org/CORBA/AttributeMode:1.0"))
    nil)
   (operation "create_operation" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/OperationDef:1.0" "OperationDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("result" :param_in "IDL:omg.org/CORBA/IDLType:1.0")
     ("mode" :param_in "IDL:omg.org/CORBA/OperationMode:1.0")
     ("params" :param_in "IDL:omg.org/CORBA/ParDescriptionSeq:1.0")
     ("exceptions" :param_in
      (:tk_alias "IDL:omg.org/CORBA/ExceptionDefSeq:1.0" "ExceptionDefSeq"
       (:tk_sequence "IDL:omg.org/CORBA/ExceptionDef:1.0" 0)))
     ("contexts" :param_in "IDL:omg.org/CORBA/ContextIdSeq:1.0"))
    nil))
  (interface "::CORBA::ExceptionDef"
   ("IDL:omg.org/CORBA/Contained:1.0" "IDL:omg.org/CORBA/Container:1.0")
   "IDL:omg.org/CORBA/ExceptionDef:1.0"
   (attribute "type" :attr_readonly (:tk_typecode))
   (attribute "members" :attr_normal "IDL:omg.org/CORBA/StructMemberSeq:1.0"))
  (interface "::CORBA::NativeDef" ("IDL:omg.org/CORBA/TypedefDef:1.0")
   "IDL:omg.org/CORBA/NativeDef:1.0")
  (type "::CORBA::InterfaceDefSeq" "IDL:omg.org/CORBA/InterfaceDefSeq:1.0")
  (interface "::CORBA::ValueDef"
   ("IDL:omg.org/CORBA/Container:1.0" "IDL:omg.org/CORBA/Contained:1.0"
    "IDL:omg.org/CORBA/IDLType:1.0")
   "IDL:omg.org/CORBA/ValueDef:1.0"
   (attribute "supported_interfaces" :attr_normal
    "IDL:omg.org/CORBA/InterfaceDefSeq:1.0")
   (attribute "initializers" :attr_normal
    "IDL:omg.org/CORBA/InitializerSeq:1.0")
   (attribute "base_value" :attr_normal "IDL:omg.org/CORBA/ValueDef:1.0")
   (attribute "abstract_base_values" :attr_normal
    "IDL:omg.org/CORBA/ValueDefSeq:1.0")
   (attribute "is_abstract" :attr_normal (:tk_boolean))
   (attribute "is_custom" :attr_normal (:tk_boolean))
   (attribute "is_truncatable" :attr_normal (:tk_boolean))
   (operation "is_a" :op_normal (:tk_boolean)
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")) nil)
   (struct "::CORBA::ValueDef::FullValueDescription"
    (:tk_struct "IDL:omg.org/CORBA/ValueDef/FullValueDescription:1.0"
     "FullValueDescription"
     (("name" "IDL:omg.org/CORBA/Identifier:1.0")
      ("id" "IDL:omg.org/CORBA/RepositoryId:1.0") ("is_abstract" (:tk_boolean))
      ("is_custom" (:tk_boolean))
      ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
      ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
      ("operations" "IDL:omg.org/CORBA/OpDescriptionSeq:1.0")
      ("attributes" "IDL:omg.org/CORBA/AttrDescriptionSeq:1.0")
      ("members"
       (:tk_alias "IDL:omg.org/CORBA/ValueMemberSeq:1.0" "ValueMemberSeq"
        (:tk_sequence
         (:tk_struct "IDL:omg.org/CORBA/ValueMember:1.0" "ValueMember"
          (("name" "IDL:omg.org/CORBA/Identifier:1.0")
           ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
           ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
           ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
           ("type" (:tk_typecode)) ("type_def" "IDL:omg.org/CORBA/IDLType:1.0")
           ("access"
            (:tk_alias "IDL:omg.org/CORBA/Visibility:1.0" "Visibility"
             (:tk_short)))))
         0)))
      ("initializers" "IDL:omg.org/CORBA/InitializerSeq:1.0")
      ("supported_interfaces" "IDL:omg.org/CORBA/RepositoryIdSeq:1.0")
      ("abstract_base_values" "IDL:omg.org/CORBA/RepositoryIdSeq:1.0")
      ("is_truncatable" (:tk_boolean))
      ("base_value" "IDL:omg.org/CORBA/RepositoryId:1.0")
      ("type" (:tk_typecode)))))
   (operation "describe_value" :op_normal
    "IDL:omg.org/CORBA/ValueDef/FullValueDescription:1.0" nil nil)
   (operation "create_value_member" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/ValueMemberDef:1.0" "ValueMemberDef")
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("type" :param_in "IDL:omg.org/CORBA/IDLType:1.0")
     ("access" :param_in "IDL:omg.org/CORBA/Visibility:1.0"))
    nil)
   (operation "create_attribute" :op_normal
    "IDL:omg.org/CORBA/AttributeDef:1.0"
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("type" :param_in "IDL:omg.org/CORBA/IDLType:1.0")
     ("mode" :param_in "IDL:omg.org/CORBA/AttributeMode:1.0"))
    nil)
   (operation "create_operation" :op_normal
    "IDL:omg.org/CORBA/OperationDef:1.0"
    (("id" :param_in "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("name" :param_in "IDL:omg.org/CORBA/Identifier:1.0")
     ("version" :param_in "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("result" :param_in "IDL:omg.org/CORBA/IDLType:1.0")
     ("mode" :param_in "IDL:omg.org/CORBA/OperationMode:1.0")
     ("params" :param_in "IDL:omg.org/CORBA/ParDescriptionSeq:1.0")
     ("exceptions" :param_in "IDL:omg.org/CORBA/ExceptionDefSeq:1.0")
     ("contexts" :param_in "IDL:omg.org/CORBA/ContextIdSeq:1.0"))
    nil))
  (type "::CORBA::ValueDefSeq" "IDL:omg.org/CORBA/ValueDefSeq:1.0")
  (interface "::CORBA::ValueBoxDef" ("IDL:omg.org/CORBA/TypedefDef:1.0")
   "IDL:omg.org/CORBA/ValueBoxDef:1.0"
   (attribute "original_type_def" :attr_normal
    "IDL:omg.org/CORBA/IDLType:1.0"))
  (interface "::CORBA::AbstractInterfaceDef"
   ("IDL:omg.org/CORBA/InterfaceDef:1.0")
   "IDL:omg.org/CORBA/AbstractInterfaceDef:1.0")
  (type "::CORBA::AbstractInterfaceDefSeq"
   "IDL:omg.org/CORBA/AbstractInterfaceDefSeq:1.0")
  (interface "::CORBA::LocalInterfaceDef"
   ("IDL:omg.org/CORBA/InterfaceDef:1.0")
   "IDL:omg.org/CORBA/LocalInterfaceDef:1.0")
  (type "::CORBA::LocalInterfaceDefSeq"
   (:tk_alias "IDL:omg.org/CORBA/LocalInterfaceDefSeq:1.0"
    "LocalInterfaceDefSeq"
    (:tk_sequence "IDL:omg.org/CORBA/LocalInterfaceDef:1.0" 0)))
  (type "::CORBA::ContainedSeq" "IDL:omg.org/CORBA/ContainedSeq:1.0")
  (struct "::CORBA::StructMember" "IDL:omg.org/CORBA/StructMember:1.0")
  (type "::CORBA::StructMemberSeq" "IDL:omg.org/CORBA/StructMemberSeq:1.0")
  (struct "::CORBA::Initializer" "IDL:omg.org/CORBA/Initializer:1.0")
  (type "::CORBA::InitializerSeq" "IDL:omg.org/CORBA/InitializerSeq:1.0")
  (struct "::CORBA::UnionMember" "IDL:omg.org/CORBA/UnionMember:1.0")
  (type "::CORBA::UnionMemberSeq" "IDL:omg.org/CORBA/UnionMemberSeq:1.0")
  (type "::CORBA::EnumMemberSeq" "IDL:omg.org/CORBA/EnumMemberSeq:1.0")
  (interface "::CORBA::PrimitiveDef" ("IDL:omg.org/CORBA/IDLType:1.0")
   "IDL:omg.org/CORBA/PrimitiveDef:1.0"
   (attribute "kind" :attr_readonly "IDL:omg.org/CORBA/PrimitiveKind:1.0"))
  (interface "::CORBA::StringDef" ("IDL:omg.org/CORBA/IDLType:1.0")
   "IDL:omg.org/CORBA/StringDef:1.0"
   (attribute "bound" :attr_normal (:tk_ulong)))
  (interface "::CORBA::SequenceDef" ("IDL:omg.org/CORBA/IDLType:1.0")
   "IDL:omg.org/CORBA/SequenceDef:1.0"
   (attribute "bound" :attr_normal (:tk_ulong))
   (attribute "element_type" :attr_readonly (:tk_typecode))
   (attribute "element_type_def" :attr_normal "IDL:omg.org/CORBA/IDLType:1.0"))
  (interface "::CORBA::ArrayDef" ("IDL:omg.org/CORBA/IDLType:1.0")
   "IDL:omg.org/CORBA/ArrayDef:1.0"
   (attribute "length" :attr_normal (:tk_ulong))
   (attribute "element_type" :attr_readonly (:tk_typecode))
   (attribute "element_type_def" :attr_normal "IDL:omg.org/CORBA/IDLType:1.0"))
  (interface "::CORBA::WstringDef" ("IDL:omg.org/CORBA/IDLType:1.0")
   "IDL:omg.org/CORBA/WstringDef:1.0"
   (attribute "bound" :attr_normal (:tk_ulong)))
  (interface "::CORBA::FixedDef" ("IDL:omg.org/CORBA/IDLType:1.0")
   "IDL:omg.org/CORBA/FixedDef:1.0"
   (attribute "digits" :attr_normal (:tk_ushort))
   (attribute "scale" :attr_normal (:tk_short)))
  (type "::CORBA::PrimitiveKind" "IDL:omg.org/CORBA/PrimitiveKind:1.0") nil nil
  (struct "::CORBA::ModuleDescription"
   (:tk_struct "IDL:omg.org/CORBA/ModuleDescription:1.0" "ModuleDescription"
    (("name" "IDL:omg.org/CORBA/Identifier:1.0")
     ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("version" "IDL:omg.org/CORBA/VersionSpec:1.0"))))
  (struct "::CORBA::ConstantDescription"
   (:tk_struct "IDL:omg.org/CORBA/ConstantDescription:1.0"
    "ConstantDescription"
    (("name" "IDL:omg.org/CORBA/Identifier:1.0")
     ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("version" "IDL:omg.org/CORBA/VersionSpec:1.0") ("type" (:tk_typecode))
     ("value" (:tk_any)))))
  (interface "::CORBA::TypedefDef"
   ("IDL:omg.org/CORBA/Contained:1.0" "IDL:omg.org/CORBA/IDLType:1.0")
   (:tk_objref "IDL:omg.org/CORBA/TypedefDef:1.0" "TypedefDef"))
  (struct "::CORBA::TypeDescription"
   (:tk_struct "IDL:omg.org/CORBA/TypeDescription:1.0" "TypeDescription"
    (("name" "IDL:omg.org/CORBA/Identifier:1.0")
     ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("version" "IDL:omg.org/CORBA/VersionSpec:1.0") ("type" (:tk_typecode)))))
  (struct "::CORBA::ExceptionDescription"
   "IDL:omg.org/CORBA/ExceptionDescription:1.0")
  (type "::CORBA::AttributeMode" "IDL:omg.org/CORBA/AttributeMode:1.0") nil nil
  (interface "::CORBA::AttributeDef" ("IDL:omg.org/CORBA/Contained:1.0")
   "IDL:omg.org/CORBA/AttributeDef:1.0"
   (attribute "type" :attr_readonly (:tk_typecode))
   (attribute "type_def" :attr_normal "IDL:omg.org/CORBA/IDLType:1.0")
   (attribute "mode" :attr_normal "IDL:omg.org/CORBA/AttributeMode:1.0"))
  (struct "::CORBA::AttributeDescription"
   "IDL:omg.org/CORBA/AttributeDescription:1.0")
  (type "::CORBA::OperationMode" "IDL:omg.org/CORBA/OperationMode:1.0")
  (type "::CORBA::ParameterMode" "IDL:omg.org/CORBA/ParameterMode:1.0")
  (struct "::CORBA::ParameterDescription"
   "IDL:omg.org/CORBA/ParameterDescription:1.0")
  (type "::CORBA::ParDescriptionSeq" "IDL:omg.org/CORBA/ParDescriptionSeq:1.0")
  (type "::CORBA::ContextIdentifier" "IDL:omg.org/CORBA/ContextIdentifier:1.0")
  (type "::CORBA::ContextIdSeq" "IDL:omg.org/CORBA/ContextIdSeq:1.0")
  (type "::CORBA::ExceptionDefSeq" "IDL:omg.org/CORBA/ExceptionDefSeq:1.0")
  (type "::CORBA::ExcDescriptionSeq" "IDL:omg.org/CORBA/ExcDescriptionSeq:1.0")
  (interface "::CORBA::OperationDef" ("IDL:omg.org/CORBA/Contained:1.0")
   "IDL:omg.org/CORBA/OperationDef:1.0"
   (attribute "result" :attr_readonly (:tk_typecode))
   (attribute "result_def" :attr_normal "IDL:omg.org/CORBA/IDLType:1.0")
   (attribute "params" :attr_normal "IDL:omg.org/CORBA/ParDescriptionSeq:1.0")
   (attribute "mode" :attr_normal "IDL:omg.org/CORBA/OperationMode:1.0")
   (attribute "contexts" :attr_normal "IDL:omg.org/CORBA/ContextIdSeq:1.0")
   (attribute "exceptions" :attr_normal
    "IDL:omg.org/CORBA/ExceptionDefSeq:1.0"))
  (struct "::CORBA::OperationDescription"
   "IDL:omg.org/CORBA/OperationDescription:1.0")
  (type "::CORBA::RepositoryIdSeq" "IDL:omg.org/CORBA/RepositoryIdSeq:1.0")
  (type "::CORBA::OpDescriptionSeq" "IDL:omg.org/CORBA/OpDescriptionSeq:1.0")
  (type "::CORBA::AttrDescriptionSeq"
   "IDL:omg.org/CORBA/AttrDescriptionSeq:1.0")
  (struct "::CORBA::InterfaceDescription"
   (:tk_struct "IDL:omg.org/CORBA/InterfaceDescription:1.0"
    "InterfaceDescription"
    (("name" "IDL:omg.org/CORBA/Identifier:1.0")
     ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("base_interfaces" "IDL:omg.org/CORBA/RepositoryIdSeq:1.0"))))
  (type "::CORBA::Visibility" "IDL:omg.org/CORBA/Visibility:1.0")
  (const "::CORBA::PRIVATE_MEMBER" "IDL:omg.org/CORBA/Visibility:1.0" 0)
  (const "::CORBA::PUBLIC_MEMBER" "IDL:omg.org/CORBA/Visibility:1.0" 1)
  (struct "::CORBA::ValueMember" "IDL:omg.org/CORBA/ValueMember:1.0")
  (type "::CORBA::ValueMemberSeq" "IDL:omg.org/CORBA/ValueMemberSeq:1.0")
  (interface "::CORBA::ValueMemberDef" ("IDL:omg.org/CORBA/Contained:1.0")
   "IDL:omg.org/CORBA/ValueMemberDef:1.0"
   (attribute "type" :attr_readonly (:tk_typecode))
   (attribute "type_def" :attr_normal "IDL:omg.org/CORBA/IDLType:1.0")
   (attribute "access" :attr_normal "IDL:omg.org/CORBA/Visibility:1.0"))
  (struct "::CORBA::ValueDescription"
   (:tk_struct "IDL:omg.org/CORBA/ValueDescription:1.0" "ValueDescription"
    (("name" "IDL:omg.org/CORBA/Identifier:1.0")
     ("id" "IDL:omg.org/CORBA/RepositoryId:1.0") ("is_abstract" (:tk_boolean))
     ("is_custom" (:tk_boolean))
     ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
     ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
     ("supported_interfaces" "IDL:omg.org/CORBA/RepositoryIdSeq:1.0")
     ("abstract_base_values" "IDL:omg.org/CORBA/RepositoryIdSeq:1.0")
     ("is_truncatable" (:tk_boolean))
     ("base_value" "IDL:omg.org/CORBA/RepositoryId:1.0")))))))
