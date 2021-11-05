/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

// See LICENSE.SiFive for license details.

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{HasXSParameter, XSBundle, XSModule}

// this file contains common building blocks that can be shared by ICache and DCache
// this is the common parameter base for L1 ICache and L1 DCache
trait L1CacheParameters {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def blockBytes:    Int
  val pageSize = 4 * 1024
}

trait HasL1CacheParameters extends HasXSParameter
  with MemoryOpConstants {
  val cacheParams: L1CacheParameters
  // nSets = 256
  def nSets = cacheParams.nSets
  // nWays = 8
  def nWays = cacheParams.nWays
  // blockBytes = 64
  def blockBytes = cacheParams.blockBytes
  // refillBytes = 256 / 8 = 32
  def refillBytes = l1BusDataWidth / 8
  // blockBits = 64 * 8 = 512
  def blockBits = blockBytes * 8
  // idxBits= $clog2(256) = 8
  def idxBits = log2Up(cacheParams.nSets)
  def wayBits = log2Up(nWays)
  // blockOffBits = $clog2(64) = 6
  def blockOffBits = log2Up(cacheParams.blockBytes)
  // refillOffBits = $clog2(256) = 8, you need 2 refill operations to write a whole cache_line
  def refillOffBits = log2Up(l1BusDataWidth / 8)
  // untagBits = 6 + 8 = 14
  def untagBits = blockOffBits + idxBits
  // 4K page
  def pgIdxBits = 12
  // pgUntagBits = min(12, 14) = 12
  def pgUntagBits = untagBits min pgIdxBits
  // tagBits = 36 - 12 = 24
  def tagBits = PAddrBits - pgUntagBits

  // the basic unit at which we store contents
  // SRAM bank width
  // rowBits = 64
  def rowBits = cacheParams.rowBits
  // rowBytes = 64 / 8 = 8
  def rowBytes = rowBits/8
  def rowOffBits = log2Up(rowBytes)
  // the number of rows in a block
  // blockRows = 64 / 8 = 8
  def blockRows = blockBytes / rowBytes

  // outer bus width
  def beatBits = l1BusDataWidth
  def beatBytes = beatBits / 8
  def refillCycles = blockBytes / beatBytes
  def beatOffBits = log2Up(beatBytes)

  // inner bus width(determined by XLEN)
  // wordBits = DataBits = 64
  def wordBits = DataBits
  def wordBytes = wordBits / 8
  def wordOffBits = log2Up(wordBytes)
  // the number of words in a block
  // blockWords = 64 / 8 = 8
  def blockWords = blockBytes / wordBytes
  // refillWords = 32 / 8 = 4
  def refillWords = refillBytes / wordBytes

  def get_phy_tag(paddr: UInt) = (paddr >> pgUntagBits).asUInt()
  def get_tag(addr: UInt) = get_phy_tag(addr)
  def get_idx(addr: UInt) = addr(untagBits-1, blockOffBits)
  def get_untag(addr: UInt) = addr(pgUntagBits-1, 0)
  def get_block(addr: UInt) = addr >> blockOffBits
  def get_block_addr(addr: UInt) = (addr >> blockOffBits) << blockOffBits
  def get_refill_addr(addr: UInt) = (addr >> refillOffBits) << refillOffBits

  def get_beat(addr: UInt) = addr(blockOffBits - 1, beatOffBits)
  def get_row(addr: UInt) = addr(blockOffBits - 1, rowOffBits)
  def get_word(addr: UInt) = addr(blockOffBits - 1, wordOffBits)

  def beatRows = beatBits/rowBits
  def rowWords = rowBits/wordBits
  def blockBeats = blockBytes / beatBytes

  def full_divide(a: Int, b: Int) = a >= b && isPow2(a / b)
}

abstract class L1CacheModule(implicit p: Parameters) extends XSModule
  with HasL1CacheParameters

abstract class L1CacheBundle(implicit p: Parameters) extends XSBundle
  with HasL1CacheParameters
