package abalone

import "testing"

type isValidTest struct {
	m Move
	g Game
	v bool
}

var isValidTests = []isValidTest{}

type inlineTest struct {
	m Move
	v bool
}

var inlineTests = []inlineTest{}

type inlineMovedTest struct {
	m Move
	b Board
	v []Hex
}

var inlineMovedTests = []inlineMovedTest{}

func Test_isValid(t *testing.T) {
	for _, test := range isValidTests {
		if test.m.isValid(&test.g) != test.v {
			t.Error("isValid: For", test.m, test.g, "expected", test.v)
		}
	}
}

func Test_inline(t *testing.T) {
	for _, test := range inlineTests {
		if test.m.inline() != test.v {
			t.Error("inline: For", test.m, "expected", test.v)
		}
	}
}

func Test_inlineMoved(t *testing.T) {
	for _, test := range inlineMovedTests {
		actual := test.m.inlineMoved(test.b)
		if !hexesEq(actual, test.v) {
			t.Error("isValid: For", test.m, test.b, "expected", test.v, "got", actual)
		}
	}
}
