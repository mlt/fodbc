<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version="1.0">

  <xsl:template name="extra">
  function SQLBindCol<xsl:value-of select="@suffix" /> &amp;
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    <xsl:value-of select="@fortran" />,target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,<xsl:value-of select="@sql" />, &amp;
        c_loc(TargetValue),sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindCol<xsl:value-of select="@suffix" />

  function SQLGetData<xsl:value-of select="@suffix" /> &amp;
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    <xsl:value-of select="@fortran" />,target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,<xsl:value-of select="@sql" />, &amp;
        c_loc(TargetValue),sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetData<xsl:value-of select="@suffix" />

  function SQLBindParameter<xsl:value-of select="@suffix" /> &amp;
    (hstmt,ipar,fParamType,fSqlType, &amp;
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    <xsl:value-of select="@fortran" />,target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,<xsl:value-of select="@sql" />,fSqlType, &amp;
    cbColDef,ibScale,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter<xsl:value-of select="@suffix" />

  function SQLBindParameter<xsl:value-of select="@suffix" />_ &amp;
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    <xsl:value-of select="@fortran" />,target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,<xsl:value-of select="@sql" />, &amp;
    <xsl:value-of select="@sql" />, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter<xsl:value-of select="@suffix" />_

  function SQLBindParameter<xsl:value-of select="@suffix" />__ &amp;
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    <xsl:value-of select="@fortran" />,target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,<xsl:value-of select="@sql" />, &amp;
    <xsl:value-of select="@sql" />, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter<xsl:value-of select="@suffix" />__
</xsl:template>

</xsl:stylesheet>
