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
	m.PlaceDummyBits(0)
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
