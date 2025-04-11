// main.go
package main

import (
	"fmt"
	"log"
	"math/rand"
	"os"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

const (
	// Layout constants
	GRID_LINES            = 30 // How many lines in the grid display
	CHARS_PER_GRID_COLUMN = 12 // How many characters per *visual* column
	NUM_GRID_COLUMNS      = 2  // We want two columns of characters side-by-side
	TOTAL_CHARS_PER_LINE  = CHARS_PER_GRID_COLUMN * NUM_GRID_COLUMNS
	HEX_ADDR_START        = 0xF964                // Starting hex address (example)
	HEX_ADDR_INCREMENT    = CHARS_PER_GRID_COLUMN // Increment per *visual* column
	LOG_WIDTH             = 25                    // Fixed width for the right-side log panel
	HEX_COL_WIDTH         = 8                     // Width for "0xXXXX  "

	// Gameplay constants
	PASSWORD_LENGTH = 5  // Example length for passwords (adjust as needed)
	NUM_PASSWORDS   = 12 // Example number of passwords (adjust as needed)
	NUM_BRACKETS    = 10 // Example number of bracket pairs
)

var (
	// Colors (approximating Fallout terminal)
	darkGreen  = lipgloss.Color("#18391f") // Dark background
	lightGreen = lipgloss.Color("#44ff5d") // Light text/foreground

	// Styles
	baseStyle = lipgloss.NewStyle().
			Foreground(lightGreen)

	// Style for the whole view box - ENSURE BACKGROUND COVERS ALL
	viewStyle = lipgloss.NewStyle().
			Foreground(lightGreen). // Default text color for the view
			Padding(0, 1)

	// Style for the cursor block
	cursorStyle = lipgloss.NewStyle().
			Background(lightGreen).
			Foreground(darkGreen)

	// Style for a currently selected word/bracket
	selectedStyle = lipgloss.NewStyle().
			Background(lipgloss.Color("#97ffac")).
			Foreground(darkGreen)

	// Style for used brackets or dud words
	dudStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#0a6923")). // Dimmer green
			Strikethrough(true)

	// Style for the log pane
	logPaneStyle = lipgloss.NewStyle().
			Width(LOG_WIDTH).
			Padding(0, 1).
			Align(lipgloss.Left) // Align log text left

	// Style for the hex address column
	hexStyle = lipgloss.NewStyle().
			Foreground(lightGreen).
			Width(HEX_COL_WIDTH).
			Align(lipgloss.Left) // Align hex addresses left
)

// --- Game Logic Types ---
type GameState int

const (
	Playing GameState = iota
	Won
	Lost
)

type Selectable struct {
	Value      string
	StartIndex int
	EndIndex   int
	IsWord     bool
	IsDud      bool
	IsUsed     bool
}

// --- Bubble Tea Model ---
type model struct {
	screenWidth     int
	screenHeight    int
	gameState       GameState
	words           []string
	correctPassword string
	attemptsLeft    int
	maxAttempts     int
	likeness        int
	log             []string
	gridChars       []rune // Flat array: (GRID_LINES * TOTAL_CHARS_PER_LINE)
	selectables     []Selectable
	cursorIndex     int // Index within the flat gridChars
	selectedIndex   int // Index in the selectables slice (-1 if none)
	gridHeight      int // Store GRID_LINES for convenience
}

// --- initialModel ---
func initialModel() model {
	gridHeight := GRID_LINES
	gridSize := gridHeight * TOTAL_CHARS_PER_LINE // Use total characters per logical line

	words := generateWords(NUM_PASSWORDS, PASSWORD_LENGTH)
	if len(words) == 0 {
		log.Fatal("Error: No words generated. Check word list and length.")
	}
	correctPassword := words[rand.Intn(len(words))]

	gridChars := make([]rune, gridSize)
	selectables := make([]Selectable, 0)

	// --- Grid Generation ---
	fillWithJunk(gridChars)
	// Pass TOTAL_CHARS_PER_LINE as the width for placement logic
	placeWords(gridChars, words, &selectables, TOTAL_CHARS_PER_LINE)
	placeBrackets(gridChars, NUM_BRACKETS, &selectables, TOTAL_CHARS_PER_LINE)
	// --- End Grid Generation ---

	return model{
		gameState:       Playing,
		words:           words,
		correctPassword: correctPassword,
		maxAttempts:     4,
		attemptsLeft:    4,
		likeness:        -1,
		log:             []string{"ROBCO Industries (TM) Termlink", "Password Required."},
		gridChars:       gridChars,
		selectables:     selectables,
		cursorIndex:     0,
		selectedIndex:   -1,
		gridHeight:      gridHeight,
	}
}

// --- Placeholder Generation Functions ---

func fillWithJunk(grid []rune) {
	// Increased density of symbols closer to Fallout
	junk := "!@#$%^&*()_+-=[]{};':\",./<>?|\\`~" +
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ" + // Add some letters
		"1234567890" // Add numbers
	for i := range grid {
		grid[i] = rune(junk[rand.Intn(len(junk))])
	}
}

func generateWords(count, length int) []string {
	// Load from a file or use a larger embedded list in a real app
	candidates := []string{
		"HACKING", "TERMINAL", "COMPUTER", "FALLOUT", "NUCLEAR", "RADIANT", "SECURE",
		"ACCESS", "SYSTEM", "LOCKOUT", "CYBER", "NET", "LOGIN", "ROBCO", "VAULT",
		"MUTANT", "GHOUL", "WASTE", "POWER", "ARMOR", "LASER", "RIFLE", "ADMIN",
		"ENTRY", "DENIED", "ALLOW", "CODES", "VIRUS", "QUERY", "DATA", "FILES",
		"USERS", "PASSWD", "SECRET", "HIDDEN", "LEVEL", "DELTA", "GAMMA", "OMEGA",
		"SIERRA", "TANGO", "WHISKY", "ZULU", "ALPHA", "BRAVO", "CHARLIE", "ECHO",
		"HOTEL", "INDIA", "JULIET", "PAPA", "QUEBEC", "ROMEO", "VICTOR", "YANKEE",
		"TERRA", "AQUA", "IGNIS", "VENTUS", "CAESAR", "LEGION", "NCR", "ENCLAVE",
	}
	result := []string{}
	valid := []string{}
	for _, w := range candidates {
		if len(w) == length {
			valid = append(valid, strings.ToUpper(w)) // Ensure uppercase
		}
	}
	if len(valid) == 0 {
		fmt.Printf("Warning: No words found with length %d. Using fallback.\n", length)
		return []string{strings.Repeat("A", length)} // Ensure at least one word exists
	}
	rand.Shuffle(len(valid), func(i, j int) { valid[i], valid[j] = valid[j], valid[i] })
	for i := 0; i < count && i < len(valid); i++ {
		result = append(result, valid[i])
	}
	return result
}

// Helper to check if a range overlaps with existing selectables
func overlaps(start, end int, selectables *[]Selectable) bool {
	for _, s := range *selectables {
		// Check for any overlap: (StartA <= EndB) and (EndA >= StartB)
		if start <= s.EndIndex && end >= s.StartIndex {
			return true
		}
	}
	return false
}

func placeWords(grid []rune, words []string, selectables *[]Selectable, width int) {
	// width here is TOTAL_CHARS_PER_LINE
	charsPerCol := CHARS_PER_GRID_COLUMN // Get the visual column width

	for _, word := range words {
		placed := false
		attempts := 0
		maxAttempts := 300 // Maybe increase attempts slightly

		for !placed && attempts < maxAttempts {
			attempts++
			startIdx := rand.Intn(len(grid) - len(word))
			endIdx := startIdx + len(word) - 1

			// Check 1: Does it wrap around the LOGICAL line? (Existing check)
			if (startIdx / width) != (endIdx / width) {
				continue // Wraps logical line, invalid position
			}

			// Check 2: Does it wrap around a VISUAL column boundary? (NEW CHECK)
			startVisualCol := (startIdx % width) / charsPerCol
			endVisualCol := (endIdx % width) / charsPerCol
			if startVisualCol != endVisualCol {
				continue // Wraps visual column, invalid position
			}

			// Check 3: Does it overlap existing selectables? (Existing check)
			if overlaps(startIdx, endIdx, selectables) {
				continue // Overlaps, try again
			}

			// Place the word
			for i, r := range word {
				grid[startIdx+i] = r
			}
			*selectables = append(*selectables, Selectable{
				Value:      word,
				StartIndex: startIdx,
				EndIndex:   endIdx,
				IsWord:     true,
			})
			placed = true
		}
		if !placed {
			fmt.Printf("Warning: Could not place word '%s' after %d attempts.\n", word, maxAttempts)
		}
	}
}

func placeBrackets(grid []rune, count int, selectables *[]Selectable, width int) {
	// width here is TOTAL_CHARS_PER_LINE
	charsPerCol := CHARS_PER_GRID_COLUMN // Get the visual column width
	bracketPairs := []string{"()", "[]", "{}", "<>"}
	placedCount := 0
	attempts := 0
	maxAttempts := 500

	for placedCount < count && attempts < maxAttempts {
		attempts++
		pair := bracketPairs[rand.Intn(len(bracketPairs))]
		openBracket, closeBracket := rune(pair[0]), rune(pair[1])

		// Find a random potential start position
		startIdx := rand.Intn(len(grid))

		// Make sure start position isn't already part of something
		if overlaps(startIdx, startIdx, selectables) || grid[startIdx] == openBracket || grid[startIdx] == closeBracket {
			continue
		}

		// Check if start is within a valid visual column boundary (optional but good practice)
		// Note: A single char cannot cross a boundary itself, but ensures we calculate startVisualCol correctly.
		startVisualCol := (startIdx % width) / charsPerCol

		// Search forward for a suitable closing position on the same logical line
		lineStart := (startIdx / width) * width
		lineEnd := lineStart + width - 1
		foundEnd := -1
		searchAttempts := 0
		maxSearchDist := width / 2 // Limit search distance

		for currentPos := startIdx + 1; currentPos <= lineEnd && searchAttempts < maxSearchDist; currentPos++ {
			searchAttempts++

			// Check if position is suitable for closing bracket
			if !overlaps(currentPos, currentPos, selectables) && grid[currentPos] != openBracket && grid[currentPos] != closeBracket {

				// CHECK VISUAL COLUMN for the potential end position (NEW CHECK)
				endVisualCol := (currentPos % width) / charsPerCol
				if startVisualCol != endVisualCol {
					// This potential end position is in a different visual column, skip it
					continue
				}

				// Potential end position is valid and in the same visual column
				if currentPos > startIdx+2 { // Ensure some distance
					foundEnd = currentPos
					break
				}
			}
		}

		if foundEnd != -1 {
			// We found a potential pair location in the same visual column!
			// Double check overlap for the closing bracket just to be safe
			if overlaps(foundEnd, foundEnd, selectables) {
				continue
			}

			// Place the brackets
			grid[startIdx] = openBracket
			grid[foundEnd] = closeBracket

			// Add the selectable
			*selectables = append(*selectables, Selectable{
				Value:      pair,
				StartIndex: startIdx,
				EndIndex:   foundEnd,
				IsWord:     false,
			})
			placedCount++
		}
	}
	if placedCount < count {
		fmt.Printf("Warning: Only placed %d/%d bracket pairs after %d attempts.\n", placedCount, count, attempts)
	}
}

// --- findSelectableAt Helper ---
func (m *model) findSelectableAt(cursorIdx int) int {
	for i, s := range m.selectables {
		// For words, check within the full range
		if s.IsWord && cursorIdx >= s.StartIndex && cursorIdx <= s.EndIndex {
			return i
		}
		// For brackets, check if cursor is exactly on start or end index
		if !s.IsWord && (cursorIdx == s.StartIndex || cursorIdx == s.EndIndex) {
			return i
		}
	}
	return -1
}

// --- Init ---
func (m model) Init() tea.Cmd {
	rand.Seed(time.Now().UnixNano())
	// Update selected index based on initial cursor position
	// Need to pass a pointer to modify the model directly here
	// Or return the modified model (more idiomatic in Bubble Tea)
	// Let's modify the initialModel return slightly:
	// newModel := initialModel()
	// newModel.selectedIndex = newModel.findSelectableAt(newModel.cursorIndex)
	// return newModel // In main(), or have Init return this logic
	// FOR NOW: Assume initial cursor 0 selects nothing unless a selectable starts there.
	m.selectedIndex = m.findSelectableAt(m.cursorIndex) // Add this call here
	return nil
}

// --- Update ---
func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.screenWidth = msg.Width
		m.screenHeight = msg.Height

	case tea.KeyMsg:
		if m.gameState != Playing {
			if msg.String() == "ctrl+c" || msg.String() == "q" {
				return m, tea.Quit
			}
			return m, nil
		}

		originalCursor := m.cursorIndex

		switch msg.String() {
		case "ctrl+c", "q":
			return m, tea.Quit

		case "up":
			if m.cursorIndex >= TOTAL_CHARS_PER_LINE {
				m.cursorIndex -= TOTAL_CHARS_PER_LINE // Use total width
			}
		case "down":
			if m.cursorIndex < len(m.gridChars)-TOTAL_CHARS_PER_LINE {
				m.cursorIndex += TOTAL_CHARS_PER_LINE // Use total width
			}
		case "left":
			if m.cursorIndex > 0 {
				// Prevent wrapping leftwards
				if m.cursorIndex%TOTAL_CHARS_PER_LINE != 0 {
					m.cursorIndex--
				}
			}
		case "right":
			if m.cursorIndex < len(m.gridChars)-1 {
				// Prevent wrapping rightwards
				if (m.cursorIndex+1)%TOTAL_CHARS_PER_LINE != 0 {
					m.cursorIndex++
				}
			}
		case "enter":
			if m.selectedIndex != -1 {
				// Important: Get a mutable copy of the selectable item
				// This doesn't work directly as slices hold copies.
				// We need to modify m.selectables[m.selectedIndex]
				itemIndex := m.selectedIndex // Store index before potentially changing selection

				if m.selectables[itemIndex].IsWord && !m.selectables[itemIndex].IsDud {
					// --- Process Word Guess ---
					m.attemptsLeft--
					guess := m.selectables[itemIndex].Value
					m.log = append(m.log, fmt.Sprintf("> %s", guess))

					if guess == m.correctPassword {
						m.gameState = Won
						m.log = append(m.log, "> Exact match!")
						m.log = append(m.log, "> Access granted.")
					} else {
						m.likeness = calculateLikeness(guess, m.correctPassword)
						m.log = append(m.log, "> Entry denied.")
						m.log = append(m.log, fmt.Sprintf("> Likeness=%d/%d", m.likeness, len(m.correctPassword)))

						// Mark as dud THIS word that was just guessed incorrectly
						m.selectables[itemIndex].IsDud = true

						if m.attemptsLeft <= 0 {
							m.gameState = Lost
							m.log = append(m.log, "> LOCKOUT IMMINENT!")
							m.log = append(m.log, "> System locked.")
						}
					}
				} else if !m.selectables[itemIndex].IsWord && !m.selectables[itemIndex].IsUsed {
					// --- Process Bracket Selection ---
					m.log = append(m.log, fmt.Sprintf("> Brackets %s processed.", m.selectables[itemIndex].Value))
					m.selectables[itemIndex].IsUsed = true // Mark as used

					// --- Apply Bracket Effect ---
					effectApplied := false
					// 50/50 chance: remove dud or replenish tries
					if rand.Intn(2) == 0 {
						// Try to remove a dud
						dudIndices := []int{}
						for i, s := range m.selectables {
							if s.IsWord && !s.IsDud && s.Value != m.correctPassword {
								dudIndices = append(dudIndices, i)
							}
						}
						if len(dudIndices) > 0 {
							dudToRemove := dudIndices[rand.Intn(len(dudIndices))]
							m.selectables[dudToRemove].IsDud = true
							m.log = append(m.log, "> Dud removed.")
							effectApplied = true
						}
					}

					if !effectApplied { // Either failed to find dud or RNG chose attempts
						if m.attemptsLeft < m.maxAttempts {
							m.attemptsLeft = m.maxAttempts
							m.log = append(m.log, "> Attempts replenished.")
							effectApplied = true
						}
					}

					if !effectApplied {
						m.log = append(m.log, "> No effect.") // If attempts were full and no duds found
					}

				} else {
					// Tried to select already used bracket or dud word
					m.log = append(m.log, "> Selection invalid/used.")
				}
				// Clear selection after processing
				// m.selectedIndex = -1 // Optional: clear selection after enter? Or keep it highlighted? Let's keep it.
			} else {
				m.log = append(m.log, "> Select a [WORD] or (bracket).")
			}

		} // End switch msg.String()

		// If cursor moved, update the selected index
		if m.cursorIndex != originalCursor {
			m.selectedIndex = m.findSelectableAt(m.cursorIndex)
		}

	} // End switch msg.(type)

	// Trim log
	maxLogLines := m.gridHeight
	if len(m.log) > maxLogLines {
		m.log = m.log[len(m.log)-maxLogLines:]
	}

	return m, nil
}

// --- calculateLikeness Helper ---
func calculateLikeness(guess, correct string) int { /* remains the same */
	count := 0
	for i := 0; i < len(correct) && i < len(guess); i++ {
		if guess[i] == correct[i] {
			count++
		}
	}
	return count
}

// --- View ---
func (m model) View() string {
	if m.screenWidth == 0 || m.screenHeight == 0 {
		return "Initializing..."
	}

	// --- Header --- (remains mostly the same)
	header := fmt.Sprintf("ROBCO INDUSTRIES (TM) TERMLINK PROTOCOL\n!!! WARNING: LOCKOUT IMMINENT !!!\n\n%d ATTEMPT(S) LEFT: ", m.attemptsLeft)
	for i := 0; i < m.maxAttempts; i++ {
		if i < m.attemptsLeft {
			header += "■ "
		} else {
			header += "□ "
		}
	}
	header = baseStyle.Render(header) // Apply base style to header
	header += "\n\n"

	// --- Grid Pane Builder ---
	var gridRows []string
	currentAddr := HEX_ADDR_START

	for y := 0; y < m.gridHeight; y++ {
		hexAddr1Str := hexStyle.Render(fmt.Sprintf("0x%X", currentAddr))
		hexAddr2Str := hexStyle.Render(fmt.Sprintf("0x%X", currentAddr+HEX_ADDR_INCREMENT)) // Address for second column

		var col1Builder strings.Builder
		var col2Builder strings.Builder

		// Calculate start indices for this row
		rowStartIdx := y * TOTAL_CHARS_PER_LINE
		col1StartIdx := rowStartIdx
		col2StartIdx := rowStartIdx + CHARS_PER_GRID_COLUMN

		// Build Column 1
		for x := 0; x < CHARS_PER_GRID_COLUMN; x++ {
			charIdx := col1StartIdx + x
			if charIdx >= len(m.gridChars) {
				col1Builder.WriteString(" ")
				continue
			} // Bounds check
			char := m.gridChars[charIdx]
			style := m.getStyleForChar(charIdx) // Use helper func for style
			col1Builder.WriteString(style.Render(string(char)))
		}

		// Build Column 2
		for x := 0; x < CHARS_PER_GRID_COLUMN; x++ {
			charIdx := col2StartIdx + x
			if charIdx >= len(m.gridChars) {
				col2Builder.WriteString(" ")
				continue
			} // Bounds check
			char := m.gridChars[charIdx]
			style := m.getStyleForChar(charIdx) // Use helper func for style
			col2Builder.WriteString(style.Render(string(char)))
		}

		// Combine hex addresses and styled characters for the row
		// Add spacing between columns if desired lipgloss.NewStyle().Padding(0, 1).Render(" ")
		spacer := "  " // Simple string spacer
		gridRows = append(gridRows, lipgloss.JoinHorizontal(lipgloss.Top,
			hexAddr1Str, col1Builder.String(), spacer, hexAddr2Str, col2Builder.String()))
		currentAddr += TOTAL_CHARS_PER_LINE // Increment address by total chars in the logical line
	}
	gridPane := lipgloss.JoinVertical(lipgloss.Left, gridRows...)

	// --- Log Pane Builder ---
	logContent := ""
	numLogLines := m.gridHeight
	startLogIndex := 0
	if len(m.log) > numLogLines {
		startLogIndex = len(m.log) - numLogLines
	}
	// Use baseStyle for log entries
	logEntries := []string{}
	for i := startLogIndex; i < len(m.log); i++ {
		logEntries = append(logEntries, baseStyle.Render("> "+m.log[i]))
	}
	logContent = strings.Join(logEntries, "\n")

	// Apply style AFTER joining content to ensure width/height are applied correctly
	logPane := logPaneStyle.Height(m.gridHeight).Render(logContent) // Set height to match grid

	// --- Combine Panes ---
	// Ensure gridPane doesn't take excessive width if screen is huge
	// Calculate max possible grid width based on constants
	maxGridWidth := HEX_COL_WIDTH*2 + CHARS_PER_GRID_COLUMN*2 + 4 // Add some padding/spacer width
	gridPaneStyled := lipgloss.NewStyle().MaxWidth(maxGridWidth).Render(gridPane)

	mainContent := lipgloss.JoinHorizontal(lipgloss.Top, gridPaneStyled, logPane)

	// --- Final Assembly ---
	// Create the final view string, joining header and main content
	finalViewContent := lipgloss.JoinVertical(lipgloss.Left,
		header,
		mainContent,
	)

	// Add win/loss message below the main content
	switch m.gameState {
	case Won:
		finalViewContent += "\n\n" + baseStyle.Render("> Exact match! Please wait...")
	case Lost:
		finalViewContent += "\n\n" + baseStyle.Render("> TERMINAL LOCKED OUT.")
	}

	// Use Place to center the content vertically if screen is taller than needed
	// and apply the main viewStyle (background) to the entire area.
	// Calculate height needed
	// contentHeight := strings.Count(finalViewContent, "\n") + 1
	finalRendered := lipgloss.Place(
		m.screenWidth, m.screenHeight, // Use full window size
		lipgloss.Left, lipgloss.Top, // Align content top-left
		finalViewContent, // The content string
		// lipgloss.WithConditions(lipgloss.PlacePadding, lipgloss.PlacePaddingOpts{}), // Use Place options if needed
	)

	// Apply the final view styling (background) to the entire placed content
	return viewStyle.Render(finalRendered)
	// Alternative: return viewStyle.Width(m.screenWidth).Height(m.screenHeight).Render(finalViewContent)
	// Let's stick with Place as it's often better for full-screen background control.
}

// Helper function to get the style for a character index
func (m *model) getStyleForChar(charIdx int) lipgloss.Style {
	// Is cursor here? Highest priority.
	if charIdx == m.cursorIndex {
		return cursorStyle
	}

	// Is this character part of the currently selected item?
	// Check against m.selectedIndex FIRST
	if m.selectedIndex != -1 {
		item := m.selectables[m.selectedIndex]
		// Check if charIdx falls within the bounds of the selected item
		if item.IsWord && charIdx >= item.StartIndex && charIdx <= item.EndIndex {
			// Check if it's also a dud (shouldn't happen if selected, but safety check)
			if item.IsDud {
				return dudStyle
			}
			return selectedStyle
		}
		if !item.IsWord && (charIdx == item.StartIndex || charIdx == item.EndIndex) {
			// Check if it's used
			if item.IsUsed {
				return dudStyle
			}
			return selectedStyle
		}
	}

	// Is this character part of ANY selectable item (that isn't the current selection)?
	// Used for applying dud/used styles even when not selected.
	for i, s := range m.selectables {
		if i == m.selectedIndex {
			continue
		} // Skip the currently selected one

		if s.IsWord && charIdx >= s.StartIndex && charIdx <= s.EndIndex {
			if s.IsDud {
				return dudStyle
			}
			// return baseStyle // It's part of another word, but not dud/selected
		}
		if !s.IsWord && (charIdx == s.StartIndex || charIdx == s.EndIndex) {
			if s.IsUsed {
				return dudStyle
			}
			// return baseStyle // Part of other brackets, not used/selected
		}
	}

	// Default style if none of the above apply
	return baseStyle
}

// --- Main ---
func main() {
	rand.Seed(time.Now().UnixNano())

	// --- Run Program ---
	// It's often better to initialize the model outside NewProgram
	// especially if Init() needs to modify the model state like we do
	// with selectedIndex.
	m := initialModel()
	// Perform the initial selection check after creation
	m.selectedIndex = m.findSelectableAt(m.cursorIndex)

	p := tea.NewProgram(m, tea.WithAltScreen(), tea.WithMouseCellMotion()) // Enable mouse if desired
	if _, err := p.Run(); err != nil {
		log.Fatalf("Error running program: %v", err)
		os.Exit(1)
	}
}
