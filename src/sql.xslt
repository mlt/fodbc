<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:param name="win32" select="0" />
  <xsl:output method="text" />

  <xsl:template match="/GCC_XML">
    <xsl:text>! DO NOT EDIT!
! Generated from gccxml using sql.xslt
! gccxml /usr/include/sqlext.h -fxml=/tmp/sqlext.xml &amp;&amp; xsltproc sql.xslt /tmp/sqlext.xml sql.xslt &gt; fodbc.f03
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

module fodbc
  use fodbc_types
  implicit none

  interface

</xsl:text>

    <xsl:for-each select="Function[not(starts-with(@name, '_') or @name='FireVSDebugEvent')]">

      <xsl:text>    </xsl:text>
      <xsl:call-template name="type">
	<xsl:with-param name="type" select="@returns" />
      </xsl:call-template>

      <xsl:text> function </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text> &amp;&#xa;      (</xsl:text>

      <xsl:for-each select="Argument">
	<xsl:choose>
	  <xsl:when test="@name">
	    <xsl:value-of select="@name" />
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="concat('var', position())" />
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:if test="position() != last()">
          <xsl:text>,</xsl:text>
	  <xsl:if test="position() mod 5 = 0">
	    <xsl:text> &amp;&#xa;      </xsl:text>
	  </xsl:if>
	</xsl:if>
      </xsl:for-each>

      <xsl:text>) &amp;&#xa;      bind(C, name="</xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>")&#xa;      use, intrinsic :: iso_c_binding&#xa;</xsl:text>
      <xsl:if test="$win32">
	<xsl:text>      !GCC$ ATTRIBUTES STDCALL :: </xsl:text>
	<xsl:value-of select="@name" />
	<xsl:text>&#xa;</xsl:text>
      </xsl:if>

      <xsl:for-each select="Argument">

	<xsl:text>      </xsl:text>
	<xsl:call-template name="type">
	  <xsl:with-param name="type" select="@type"/>
	  <xsl:with-param name="var">
	    <xsl:choose>
	      <xsl:when test="@name">
		<xsl:value-of select="@name" />
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="concat('var', position())" />
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
	</xsl:call-template>

	<xsl:text>&#xa;</xsl:text>

      </xsl:for-each>
      <xsl:text>    end function </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>&#xa;&#xa;</xsl:text>

    </xsl:for-each>
    <xsl:text>  end interface&#xa;end module fodbc&#xa;</xsl:text>

  </xsl:template>

  <xsl:variable name="char_type">character(kind=c_char)</xsl:variable>
  <xsl:variable name="ptr_type">type(c_ptr)</xsl:variable>

  <xsl:template name="type">
    <xsl:param name="type" />
    <xsl:param name="var" />
    <xsl:param name="ptr" select="0" />
    <xsl:variable name="next" select="//Typedef[@id=$type]" />
    <xsl:variable name="fun" select="//FundamentalType[@id=$type]"/>
    <xsl:variable name="nextp" select="//PointerType[@id=$type]" />
    <xsl:choose>
      <xsl:when test="$next">
	<xsl:call-template name="type">
          <xsl:with-param name="type" select="$next/@type"/>
          <xsl:with-param name="var" select="$var" />
	  <xsl:with-param name="ptr" select="$ptr" />
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$fun">
	<xsl:variable name="t">
	  <xsl:call-template name="translate">
	    <xsl:with-param name="type" select="$fun/@name" />
	  </xsl:call-template>
	</xsl:variable>
	<xsl:value-of select="$t" />
	<xsl:if test="$var">
	  <xsl:choose>
	    <xsl:when test="$ptr>1 or ($ptr=1 and $t!=$ptr_type and $t!=$char_type)">
	      <xsl:text>,intent(out)</xsl:text>
	    </xsl:when>
	    <xsl:when test="$t=$char_type">
	      <xsl:text>,dimension(*),intent(in)</xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>,intent(in),value</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:text> :: </xsl:text>
	  <xsl:value-of select="$var" />
	</xsl:if>
      </xsl:when>
      <xsl:when test="$nextp">
	<xsl:call-template name="type">
	  <xsl:with-param name="type" select="$nextp/@type" />
	  <xsl:with-param name="var" select="$var" />
	  <xsl:with-param name="ptr" select="$ptr + 1"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:message>Can't find definition of <xsl:value-of select="$type"/></xsl:message>
	<xsl:value-of select="$type" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="translate">
    <xsl:param name="type"/>
    <xsl:choose>
      <xsl:when test="$type='short int' or $type='short unsigned int'">
	<xsl:text>integer(kind=c_short)</xsl:text>
      </xsl:when>
      <xsl:when test="$type='long int' or $type='long unsigned int'">
	<xsl:text>integer(kind=c_long)</xsl:text>
      </xsl:when>
      <xsl:when test="$type='int'">
	<xsl:text>integer(kind=c_int)</xsl:text>
      </xsl:when>
      <xsl:when test="$type='char' or $type='unsigned char'">
	<xsl:value-of select="$char_type" />
      </xsl:when>
      <xsl:when test="$type='void'">
	<xsl:value-of select="$ptr_type" />
      </xsl:when>
      <xsl:otherwise>
	<xsl:message>I don't know Fortran equivalent of <xsl:value-of select="$type"/></xsl:message>
	<xsl:value-of select="$type"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
