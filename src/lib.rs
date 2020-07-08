pub fn execute(code: &str) -> String {
    let instructions = parser::parse(code);
    let mut robot = Robot::new();
    robot.interpret(&instructions);
    let field = robot.get_field();
    field_to_string(field)
}

#[derive (PartialEq, Debug)]
enum Direction {
    Top,
    Bottom,
    Left,
    Right
}

#[derive (PartialEq, Clone, Debug)]
enum Cell {
    Visited,
    NotVisited
}

#[derive (Debug)]
pub enum Instruction {
    Step(u32),
    TurnLeft(u32),
    TurnRight(u32)
}

#[derive (Debug)]
struct Position {
    pub x: usize,
    pub y: usize
}

#[derive (Debug)]
struct Robot {
    direction: Direction,
    field: Vec<Vec<Cell>>,
    position: Position
}

impl Robot {
    fn new() -> Robot {
        Robot {
            direction: Direction::Right,
            field: vec![vec![Cell::Visited]],
            position: Position {x: 0, y: 0}
        }
    }

    fn interpret(&mut self, instructions: &[Instruction]) {
        use Instruction::*;

        for instruction in instructions.iter() {
            match *instruction {
                Step(times) =>
                    (0 .. times).for_each(|_| self.step()),
                TurnLeft(times) =>
                    (0 .. times).for_each(|_| self.turn_left()),
                TurnRight(times) =>
                    (0 .. times).for_each(|_| self.turn_right())
            }
        }
    }

    fn get_field(&mut self) -> &[Vec<Cell>] {
        &self.field
    }

    fn turn_left(&mut self) {
        use Direction::*;

        self.direction = match self.direction {
            Top => Left,
            Left => Bottom,
            Bottom => Right,
            Right => Top
        }
    }

    fn turn_right(&mut self) {
        use Direction::*;

        self.direction = match self.direction {
            Top => Right,
            Right => Bottom,
            Bottom => Left,
            Left => Top
        }
    }

    fn step(&mut self) {
        use Direction::*;

        match self.direction {
            Right => self.step_right(),
            Left => self.step_left(),
            Top => self.step_top(),
            Bottom => self.step_bottom()
        }
    }

    fn step_right(&mut self) {
        if self.position.x + 1 >= self.field[self.position.y].len() {
            for y in 0 .. self.field.len() {
                self.field[y].push(Cell::NotVisited);
            }
        }

        self.position.x += 1;
        self.mark_visited();
    }

    fn step_left(&mut self) {
        if self.position.x == 0 {
            for y in 0 .. self.field.len() {
                self.field[y].insert(0, Cell::NotVisited);
            }
        } else {
            self.position.x -= 1;
        }

        self.mark_visited();
    }

    fn step_bottom(&mut self) {
        if self.position.y + 1 >= self.field.len() {
            self.field.push(
                vec![Cell::NotVisited; self.field[self.position.y].len()]
            )
        }

        self.position.y += 1;
        self.mark_visited();
    }

    fn step_top(&mut self) {
        if self.position.y == 0 {
            self.field.insert(0,
                vec![Cell::NotVisited; self.field[0].len()]
            );
        } else {
            self.position.y -= 1;
        }

        self.mark_visited();
    }

    fn mark_visited(&mut self) {
        self.field[self.position.y][self.position.x] = Cell::Visited;
    }
}

pub mod parser {
    use super::*;

    pub fn parse(input: &str) -> Vec<Instruction> {
        let mut instructions = vec![];
        let mut input = input;

        while let Some((next_input, instruction)) = parser::instruction(input) {
            instructions.push(instruction);
            input = next_input;
        }

        instructions
    }

    fn number(input: &str) -> Option<(&str, u32)> {
        let mut matched = String::new();
        let mut chars = input.chars();

        match chars.next() {
            Some(ch) => {
                if ch.is_numeric() {
                    matched.push(ch);
                } else {
                    return None;
                }
            },
            None => {
                return None;
            }
        }

        while let Some(ch) = chars.next() {
            if ch.is_numeric() {
                matched.push(ch);
            } else {
                break;
            }
        }

        let next_index = matched.len();
        let num = matched.parse::<u32>().unwrap();
        Some((&input[next_index .. ], num))
    }

    fn char(ch: char)
        -> impl Fn(&str) -> Option<(&str, char)>
    {
        move |input| {
            match input.chars().next() {
                Some(input_ch) => if input_ch == ch {
                    Some((&input[1..], ch))
                } else {
                    None
                },
                None => None
            }
        }
    }

    fn pair<P1, P2, R1, R2>(parser1: P1, parser2: P2)
        -> impl Fn(&str) -> Option<(&str, (R1, R2))>
    where
        P1: Fn(&str) -> Option<(&str, R1)>,
        P2: Fn(&str) -> Option<(&str, R2)>,
    {
        move |input| match parser1(input) {
            Some((next_input, result1)) => match parser2(next_input) {
                Some((final_input, result2)) => Some((final_input, (result1, result2))),
                None => None,
            },
            None => None,
        }
    }

    fn instruction(input: &str) -> Option<(&str, Instruction)> {
        use Instruction::*;

        let step_parser = move |input| {
            char('F')(input)
                .map(|(next_input, _)| (next_input, Step(1)))
        };

        let turn_left_parser = move |input| {
            char('L')(input)
                .map(|(next_input, _)| (next_input, TurnLeft(1)))
        };

        let turn_right_parser = move |input| {
            char('R')(input)
                .map(|(next_input, _)| (next_input, TurnRight(1)))
        };

        let step_n_parser = move |input| {
            pair(char('F'), number)(input)
                .map(|(next_input, (_, n))| (next_input, Step(n)))
        };

        let turn_left_n_parser = move |input| {
            pair(char('L'), number)(input)
                .map(|(next_input, (_, n))| (next_input, TurnLeft(n)))
        };

        let turn_right_n_parser = move |input| {
            pair(char('R'), number)(input)
                .map(|(next_input, (_, n))| (next_input, TurnRight(n)))
        };

        step_n_parser(input)
            .or(turn_left_n_parser(input))
            .or(turn_right_n_parser(input))
            .or(step_parser(input))
            .or(turn_left_parser(input))
            .or(turn_right_parser(input))
    }

}

fn field_to_string(field: &[Vec<Cell>]) -> String {
    use Cell::*;

    field.iter()
        .map(|row| row.iter()
            .map(|cell|
                match *cell {
                    Visited => '*',
                    NotVisited => ' '
            })
            .collect::<String>()
        )
        .collect::<Vec<String>>()
        .join("\r\n")
}

#[cfg(test)]
macro_rules! expect_equal {
  ($arg:expr, $expected:expr $(,)*) => {{
    let arg = $arg;
    let expected = $expected;
    let actual = execute(arg);
    assert_eq!(actual, expected, "\n\ngot:\n{}\n\nexpected:\n{}\n\ncmd:{}\n", actual, expected, arg);
  }};
}

#[cfg(test)]
mod tests {
use super::execute;
#[test]
fn examples_in_description() {
  expect_equal!("", "*");
  expect_equal!("FFFFF", "******");
  expect_equal!(
    "FFFFFLFFFFFLFFFFFLFFFFFL",
    "******\r\n*    *\r\n*    *\r\n*    *\r\n*    *\r\n******",
  );
  expect_equal!(
    "LFFFFFRFFFRFFFRFFFFFFF",
    "    ****\r\n    *  *\r\n    *  *\r\n********\r\n    *   \r\n    *   ",
  );
  expect_equal!(
    "LF5RF3RF3RF7",
    "    ****\r\n    *  *\r\n    *  *\r\n********\r\n    *   \r\n    *   ",
  );
}
}
