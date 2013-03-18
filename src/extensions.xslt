<?xml version="1.0"?>
<!--
TODO: Generate this XSLT from sql.xml to avoid hardcoding

For this we need XSLT 2 that is not supported by xsltproc
 -->

<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" />

  <xsl:template match="/types"><xsl:text>! DO NOT EDIT! Generated file.
!
! This file is a part of fodbc, Fortran 2003 ODBC bindings
! Copyright (C) 2013 Mikhail Titov
!
! fodbc is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as
! published by the Free Software Foundation, either version 3 of the
! License, or (at your option) any later version.
!
! fodbc is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with fodbc.  If not, see &lt;http://www.gnu.org/licenses/&gt;.

module fodbc_ext
  use fodbc, SQLBindCol0 => SQLBindCol, SQLGetData0 => SQLGetData
  use fodbc, SQLBindParameter0 => SQLBindParameter
  implicit none

</xsl:text>

  <xsl:call-template name="decl">
    <xsl:with-param name="fun" select="'SQLBindCol'" />
  </xsl:call-template>

  <xsl:text>&#xa;</xsl:text>

  <xsl:call-template name="decl">
    <xsl:with-param name="fun" select="'SQLGetData'" />
  </xsl:call-template>

  <xsl:text>&#xa;</xsl:text>

  <xsl:call-template name="decl">
    <xsl:with-param name="fun" select="'SQLBindParameter'" />
  </xsl:call-template>

  <xsl:text>&#xa;contains&#xa;</xsl:text>

<xsl:for-each select="type">
  <xsl:call-template name="SQLBindCol" />
  <xsl:call-template name="SQLGetData" />
  <xsl:call-template name="SQLBindParameter" />
</xsl:for-each>

end module fodbc_ext
  </xsl:template>

  <xsl:template name="decl">
    <xsl:param name="fun" />
    <xsl:text>  interface </xsl:text>
    <xsl:value-of select="$fun" />
    <xsl:text>&#xa;</xsl:text>
    <xsl:for-each select="type">
      <xsl:text>     module procedure </xsl:text>
      <xsl:value-of select="$fun" />_<xsl:value-of select="@sql" />
      <xsl:text>&#xa;</xsl:text>
    </xsl:for-each>
    <xsl:text>     procedure </xsl:text>
    <xsl:value-of select="$fun" />
    <xsl:text>0&#xa;</xsl:text>
    <xsl:text>  end interface </xsl:text>
    <xsl:value-of select="$fun" />
    <xsl:text>&#xa;</xsl:text>
  </xsl:template>

  <xsl:template name="SQLBindCol">
  function SQLBindCol_<xsl:value-of select="@sql" /> &amp;
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    <xsl:value-of select="@fortran" />,target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,<xsl:value-of select="@sql" />, &amp;
        c_loc(TargetValue),sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindCol_<xsl:value-of select="@sql" /><xsl:text>&#xa;</xsl:text>
  </xsl:template>

  <xsl:template name="SQLGetData">
  function SQLGetData_<xsl:value-of select="@sql" /> &amp;
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    <xsl:value-of select="@fortran" />,target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,<xsl:value-of select="@sql" />, &amp;
        c_loc(TargetValue),sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetData_<xsl:value-of select="@sql" /><xsl:text>&#xa;</xsl:text>
  </xsl:template>

  <xsl:template name="SQLBindParameter">
  function SQLBindParameter_<xsl:value-of select="@sql" /> &amp;
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
  end function SQLBindParameter_<xsl:value-of select="@sql" />
  </xsl:template>

</xsl:stylesheet>
