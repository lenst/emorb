
(:tk_struct id name ((member-name type)*))

(:tk_enum id name (member-name*))

(:tk_objref id name)

(:tk_alias id name content-type)

(:tk_union id name discriminator-type default-index ((member-label member-name member-type)*))
 where default-index is index of member that is the default or -1 if no default
       member-label is of type discriminator-type

(:tk_value id name ValueModifier concrete-base
           ((member-name member-type visibility)*))

(:tk_value_box id name content-type)

(:tk_native id name)

(:tk_local_interface id name)

(:tk_except id name ((member-name type)*))

(:tk_abstract_interface id name)

(:tk_string length)
(:tk_wstring length)

(:tk_fixed fixed_digits fixed_scale)

(:tk_sequence content-type length)
(:tk_array content-type length)

(:tk_null)
(:tk_void)
(:tk_short)
(:tk_long)
(:tk_ushort)
(:tk_ulong)
(:tk_float)
(:tk_double)
(:tk_boolean)
(:tk_char)
(:tk_octet)
(:tk_any)
(:tk_typecode)
(:tk_principal)
(:tk_longlong)
(:tk_ulonglong)
(:tk_longdouble)
(:tk_wchar)
