package main

import (
	"fmt"
)

const (
	version = 1
	size    = 21
)

type Matrix struct {
	mod  [size][size]bool
	resv [size][size]bool
}

// main function
func main() {
	m := NewMatrixV1()
	codewords := BuildCodewordsV1L([]byte("hello"))
	m.PlaceCodewords(codewords, 0)
	m.PlaceFormatInfoL(0)
	m.PrintTerminal(2)
}

// NewMatrixV1 is Build version 1 base matrix with function patterns only
func NewMatrixV1() *Matrix {
	m := &Matrix{}

	//Finder patterns
	m.placeFinder(0, 0)
	m.placeFinder(0, size-7)
	m.placeFinder(size-7, 0)

	// Separators around finders
	m.placeSeparators(0, 0)
	m.placeSeparators(0, size-7)
	m.placeSeparators(size-7, 0)

	// Timing patterns (row 6, col 6), skipping overlaps already reserved
	for i := 0; i < size; i++ {
		// row 6
		if !m.resv[6][i] {
			m.setResv(6, i, i%2 == 0)
		}
		// col 6
		if !m.resv[i][6] {
			m.setResv(i, 6, i%2 == 0)
		}
	}

	//Dark module: (4*version + 9, 8) +> (13, 8) for version = 1
	m.setResv(4*version+9, 8, true)

	// Reserve format info areas (so data placement can skip them later)
	m.reserveFormatInfo()

	return m
}

func (m *Matrix) setResv(r, c int, black bool) {
	m.mod[r][c] = black
	m.resv[r][c] = true
}

func (m *Matrix) placeFinder(r0, c0 int) {
	// 7*7 finder
	// outer black border, inner white border, center 3*3 black
	for r := 0; r < 7; r++ {
		for c := 0; c < 7; c++ {
			rr := r0 + r
			cc := c0 + c

			black := false
			switch {
			case r == 0 || r == 6 || c == 0 || c == 6:
				black = true
			case r == 1 || r == 5 || c == 1 || c == 5:
				black = false
			default:
				black = true
			}
			m.setResv(rr, cc, black)
		}
	}
}

func (m *Matrix) placeSeparators(r0, c0 int) {
	// 1-module white border around the 7*7 finder, within bounds
	for dr := -1; dr <= 7; dr++ {
		for dc := -1; dc <= 7; dc++ {
			rr := r0 + dr
			cc := c0 + dc
			if rr < 0 || rr >= size || cc < 0 || cc >= size {
				continue
			}
			if dr >= 0 && dr < 7 && dc >= 0 && dc < 7 {
				continue
			}

			// separator is white
			m.setResv(rr, cc, false)
		}
	}
}

func (m *Matrix) reserveFormatInfo() {
	// Top-left format info (row 8 col 0..8, col 8 row 0..8)
	for i := 0; i <= 8; i++ {
		m.resv[8][i] = true
		m.resv[i][8] = true
	}
	// Top-right format info copy (row 8 col size-8.size-1)
	for c := size - 8; c < size; c++ {
		m.resv[8][c] = true
	}
	// Bottm-left format info copy (col 8 row size-7..size-1)
	for r := size - 7; r < size; r++ {
		m.resv[r][8] = true
	}
}

func (m *Matrix) PrintTerminal(quiet int) {
	black := "  "
	white := "██"

	// top quiet
	for i := 0; i < quiet; i++ {
		for j := 0; j < size+2*quiet; j++ {
			fmt.Print(white)
		}
		fmt.Println()
	}

	for r := 0; r < size; r++ {
		for i := 0; i < quiet; i++ {
			fmt.Print(white)
		}
		for c := 0; c < size; c++ {
			if m.mod[r][c] {
				fmt.Print(black)
			} else {
				fmt.Print(white)
			}
		}
		for i := 0; i < quiet; i++ {
			fmt.Print(white)
		}
		fmt.Println()
	}

	// botom quiet zone
	for i := 0; i < quiet; i++ {
		for j := 0; j < size+2*quiet; j++ {
			fmt.Print(white)
		}
		fmt.Println()
	}
}

// PlaceDummyBits is zipzag placement for data modules, skipping reserved cells.
// we fill with a dummy pattern: 010101...
func (m *Matrix) PlaceDummyBits(mask int) {
	bit := false
	// Colums are processed right-toleft in pairs (col, col-1)
	for col := size - 1; col > 0; col -= 2 {
		// Skip timing column (col 6)
		if col == 6 {
			col--
		}

		// Direction alternates by column-pair: up, down, up, down...
		pairIndex := (size - 1 - col) / 2
		up := (pairIndex%2 == 0)

		if up {
			// from bottom to top
			for row := size - 1; row >= 0; row-- {
				m.placeTwo(row, col, mask, &bit)
			}
		} else {
			// from top to bottom
			for row := 0; row < size; row++ {
				m.placeTwo(row, col, mask, &bit)
			}
		}
	}
}

func (m *Matrix) placeTwo(row, col, mask int, bit *bool) {
	// Place at (row, col) then (row, col-1)
	for dx := 0; dx < 2; dx++ {
		c := col - dx
		if c < 0 {
			continue
		}
		if m.resv[row][c] {
			continue
		}

		v := *bit
		*bit = !*bit // next dummy bit

		// apply mask (only mask 0 for now)
		if maskBit(mask, row, c) {
			v = !v
		}
		m.mod[row][c] = v
	}
}

func maskBit(mask, r, c int) bool {
	// mask 0: (r + c) % 2 == 0
	switch mask {
	case 0:
		return (r+c)%2 == 0
	default:
		return (r+c)%2 == 0
	}
}

type BitStream struct {
	cw   []byte
	i    int
	bit  int
	done bool
}

func NewBitStream(codewords []byte) *BitStream {
	return &BitStream{cw: codewords}
}

func (s *BitStream) Next() bool {
	if s.done || s.i >= len(s.cw) {
		s.done = true
		return false
	}
	b := s.cw[s.i]
	v := ((b >> uint(7-s.bit)) & 1) == 1

	s.bit++
	if s.bit == 8 {
		s.bit = 0
		s.i++
	}

	return v
}

func (m *Matrix) PlaceCodewords(codewords []byte, mask int) {
	stream := NewBitStream(codewords)

	for col := size - 1; col > 0; col -= 2 {
		if col == 6 {
			col--
		}
		pairIndex := (size - 1 - col) / 2
		up := (pairIndex % 2) == 0

		if up {
			for row := size - 1; row >= 0; row-- {
				m.placeTwoFromStream(row, col, mask, stream)
			}
		} else {
			for row := 0; row < size; row++ {
				m.placeTwoFromStream(row, col, mask, stream)
			}
		}
	}
}

func (m *Matrix) placeTwoFromStream(row, col, mask int, stream *BitStream) {
	for dx := 0; dx < 2; dx++ {
		c := col - dx
		if c < 0 {
			continue
		}
		if m.resv[row][c] {
			continue
		}

		v := stream.Next()

		if maskBit(mask, row, c) {
			v = !v
		}
		m.mod[row][c] = v
	}
}

func BuildCodewordsV1L(payload []byte) []byte {
	data := makeDataCodewordsV1LByte(payload) // 19 bytes
	ecc := rsEncode(data, 7)                  // 7 bytes
	out := make([]byte, 0, 26)
	out = append(out, data...)
	out = append(out, ecc...)
	return out
}

func makeDataCodewordsV1LByte(payload []byte) []byte {
	// 容量: V1-L, Byteモードは 17バイト
	// ここは最初はpanicでもいいけど、後でerror返しにした方がいい
	if len(payload) > 17 {
		payload = payload[:17]
	}

	// ビットを貯める
	var bits []bool
	appendBits := func(val uint, n int) {
		for i := n - 1; i >= 0; i-- {
			bits = append(bits, ((val>>uint(i))&1) == 1)
		}
	}
	appendByte := func(b byte) { appendBits(uint(b), 8) }

	// モード 0100（Byte）
	appendBits(0b0100, 4)
	// 文字数（Version 1なので8bit）
	appendBits(uint(len(payload)), 8)
	// データ
	for _, b := range payload {
		appendByte(b)
	}

	// 終端（最大4bit、入るだけ）
	capBits := 19 * 8
	remain := capBits - len(bits)
	term := 4
	if remain < term {
		term = remain
	}
	appendBits(0, term)

	// バイト境界まで0で埋める
	for len(bits)%8 != 0 {
		bits = append(bits, false)
	}

	// bits→bytes
	toBytes := func(bb []bool) []byte {
		out := make([]byte, (len(bb)+7)/8)
		for i, bit := range bb {
			if bit {
				out[i/8] |= 1 << uint(7-(i%8))
			}
		}
		return out
	}
	out := toBytes(bits)

	// 残りのコードワードを 0xEC,0x11 交互で埋める
	pads := []byte{0xEC, 0x11}
	p := 0
	for len(out) < 19 {
		out = append(out, pads[p%2])
		p++
	}
	return out[:19]
}

// ---- Format Information (Version 1 has no version-info bits) ----
// EC level L: 01, mask: 3 bits
// format bits are BCH(15,5) with XOR mask 0x5412

func (m *Matrix) PlaceFormatInfoL(mask int) {
	f := formatInfoBitsL(mask) // 15 bits (MSB first)

	get := func(i int) bool {
		// i=0..14, take MSB->LSB
		return ((f >> uint(14-i)) & 1) == 1
	}

	// 1) around top-left finder
	// (8,0..5)
	for i := 0; i <= 5; i++ {
		m.mod[8][i] = get(i)
	}
	// (8,7)
	m.mod[8][7] = get(6)
	// (8,8)
	m.mod[8][8] = get(7)
	// (7..0,8) for i=8..14
	for i := 8; i < 15; i++ {
		m.mod[14-i][8] = get(i)
	}

	// 2) the other copy
	// (8,20..13) for i=0..7
	for i := 0; i < 8; i++ {
		m.mod[8][size-1-i] = get(i)
	}
	// (14..20,8) for i=8..14
	for i := 8; i < 15; i++ {
		m.mod[size-15+i][8] = get(i)
	}
}

func formatInfoBitsL(mask int) uint16 {
	// EC level bits: L=01 (2 bits)
	// 5-bit value: [EC(2)|mask(3)]
	data := (uint16(0b01) << 3) | uint16(mask&0b111) // 5 bits

	// BCH encode to 15 bits, then XOR mask 0x5412
	return bchEncodeFormat(data) ^ 0x5412
}

func bchEncodeFormat(data5 uint16) uint16 {
	// data5 is 5 bits. shift left 10 for remainder
	d := data5 << 10

	// generator polynomial for format info: 0x537 (degree 10)
	const gen uint16 = 0x537

	// polynomial long division
	for i := 14; i >= 10; i-- {
		if ((d >> uint(i)) & 1) == 1 {
			d ^= gen << uint(i-10)
		}
	}

	rem := d & 0x3FF
	return (data5 << 10) | rem
}

//rsEncode
// QR uses GF(256) with primitive polynomial 0x11D.

var gfExp [512]byte
var gfLog [256]byte

func init() {
	// Build exp/log tables.
	// gfExp[i] = α^i, gfLog[x] = i where x = α^i
	// α is 2 in this field representation.
	x := byte(1)
	for i := 0; i < 255; i++ {
		gfExp[i] = x
		gfLog[x] = byte(i)
		x = gfMulNoLUT(x, 2)
	}
	for i := 255; i < 512; i++ {
		gfExp[i] = gfExp[i-255]
	}
}

// Multiply in GF(256) without LUT (slow but only used for table generation).
func gfMulNoLUT(a, b byte) byte {
	var p byte
	for i := 0; i < 8; i++ {
		if (b & 1) != 0 {
			p ^= a
		}
		hi := (a & 0x80) != 0
		a <<= 1
		if hi {
			a ^= 0x1d // 0x11D without the x^8 term
		}
		b >>= 1
	}
	return p
}

// Fast multiply using exp/log.
func gfMul(a, b byte) byte {
	if a == 0 || b == 0 {
		return 0
	}
	return gfExp[int(gfLog[a])+int(gfLog[b])]
}

// Build RS generator polynomial for given ECC length.
// g(x) = (x - α^0)(x - α^1)...(x - α^(eccLen-1))
// In GF(256), subtraction == addition == XOR, so (x + α^i).
func rsGeneratorPoly(eccLen int) []byte {
	g := []byte{1}
	for i := 0; i < eccLen; i++ {
		a := gfExp[i] // α^i
		next := make([]byte, len(g)+1)
		for j := 0; j < len(g); j++ {
			// Multiply g by (x + a):
			// next[j]   ^= g[j] * a
			// next[j+1] ^= g[j]
			next[j] ^= gfMul(g[j], a)
			next[j+1] ^= g[j]
		}
		g = next
	}
	return g // length eccLen+1
}

// RS encode: returns eccLen bytes of parity for given data.
func rsEncode(data []byte, eccLen int) []byte {
	gen := rsGeneratorPoly(eccLen) // gen[0..eccLen], gen[0]=1
	ecc := make([]byte, eccLen)

	for _, d := range data {
		// factor = d + ecc[0] (since subtraction = addition)
		factor := d ^ ecc[0]

		// Shift left (drop ecc[0])
		copy(ecc, ecc[1:])
		ecc[eccLen-1] = 0

		// ecc = ecc + factor * gen[1..]
		for i := 0; i < eccLen; i++ {
			ecc[i] ^= gfMul(gen[i+1], factor)
		}
	}
	return ecc
}
