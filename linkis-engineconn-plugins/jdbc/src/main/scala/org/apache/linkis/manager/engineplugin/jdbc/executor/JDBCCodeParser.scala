/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.linkis.manager.engineplugin.jdbc.executor

import org.apache.linkis.governance.common.paser.{CodeParser, SQLCodeParser}
import org.apache.linkis.manager.engineplugin.jdbc.conf.JDBCConfiguration

import org.apache.commons.lang3.StringUtils

/**
 * JDBC engine SQL parser with:
 *   - robust semicolon splitting (delegate to governance SQLCodeParser)
 *   - default LIMIT injection for SELECT without LIMIT
 *   - max LIMIT enforcement (avoid huge result sets)
 */
class JDBCCodeParser extends CodeParser {

  private val delegate = new SQLCodeParser
  private val defaultLimit: Int = JDBCConfiguration.ENGINE_DEFAULT_LIMIT.getValue

  private val LimitWordRegex = """(?is)\blimit\b""".r
  private val LimitNumberRegex = """(?is)\blimit\s+(\d+)\b""".r

  override def parse(code: String): Array[String] = {
    delegate
      .parse(code)
      .map(_.trim)
      .filter(StringUtils.isNotBlank)
      .map(enforceSelectLimit)
  }

  private def enforceSelectLimit(sql: String): String = {
    if (!isSelect(sql)) return sql

    LimitNumberRegex.findFirstMatchIn(sql) match {
      case Some(m) =>
        val limitNum = m.group(1).toInt
        if (limitNum > defaultLimit) {
          throw new IllegalArgumentException(
            s"We at most allowed to limit $defaultLimit, but your SQL has been over the max rows."
          )
        }
        sql
      case None =>
        // If user already has a LIMIT keyword (e.g. LIMIT ? / LIMIT ALL / LIMIT ... OFFSET ...),
        // we don't try to rewrite it.
        if (LimitWordRegex.findFirstIn(sql).nonEmpty) sql
        else s"${stripTrailingSemicolon(sql)} limit $defaultLimit"
    }
  }

  private def stripTrailingSemicolon(sql: String): String = {
    val trimmed = sql.trim
    if (trimmed.endsWith(";")) trimmed.stripSuffix(";").trim else trimmed
  }

  private def isSelect(sql: String): Boolean = {
    val trimmed = sql.trim
    if (StringUtils.isBlank(trimmed)) return false
    val first = trimmed.split("\\s+").headOption.getOrElse("")
    "select".equalsIgnoreCase(first)
  }

}

