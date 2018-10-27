package HandRange

object RundownHands extends TypeOfHands {
  /*
Description:
4 connected cards with at most two gaps in them

Examples:
8 7 6 5
T 8 7 6
Q J 8 7
9 8 7 4

Strength component(s):
These hands are also called "rundowns" or "wraps", and they build straights.
The number and quality of the straights we build are very dependent on the number and
location of gaps in the structure.

Hwang divides this category of hands into 2 subcategories:

Premium Rundowns:
- Rundowns with no gaps (JT98)
- Rundowns with a single gap at the bottom (JT97)
- Rundowns with a single gap in the middle (JT87)

Speculative Rundowns:
- Rundowns with a double gap at the bottom (JT96)
- Rundowns with two single gaps at the bottom (JT86)
- Rundowns with a double gap in the middle (JT76)
 */

  def count: Int = {
    /* Rundowns from Q high to 8 high
     * times three for either no gaps, single gap at the bottom or single gap in the middle
     */

    // Q, J, T, 9, 8 => 5 ranks
    3 * 5 * 4 * 4 * 4 * 4
  }

  // need to add a hand like J765 double suited as well
}
