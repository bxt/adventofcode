package de.bernhardhaussner.adventofcode.year2016.day12;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.function.Function;
import java.util.regex.Pattern;

public class Main {

	private static interface Machine<R, V> {
		public void run();
		public V getResult();
		public V getRegister(R r);
		public void setRegister(R r, V v);
		public default void modifyRegister(R r, Function<V, V> f) {
			setRegister(r, f.apply(getRegister(r)));
		}
		public default void advanceInsructionPointer() {
			advanceInsructionPointer(1);
		}
		public void advanceInsructionPointer(int by);
	}

	@FunctionalInterface
	private static interface Instruction<R, V> {
		public void runOn(Machine<R, V> machine);
	}

	private abstract static class CopyInstruction<R, V> implements Instruction<R, V> {
		private R into;

		public CopyInstruction(R into) {
			this.into = into;
		}

		@Override
		public void runOn(Machine<R, V> machine) {
			machine.setRegister(into, getValue(machine));
			machine.advanceInsructionPointer();
		}

		protected abstract V getValue(Machine<R, V> machine);
	}

	private static class RegisterCopyInstruction<R, V> extends CopyInstruction<R, V> implements Instruction<R, V> {
		private R from;

		public RegisterCopyInstruction(R from, R into) {
			super(into);
			this.from = from;
		}

		@Override
		protected V getValue(Machine<R, V> machine) {
			return machine.getRegister(from);
		}
	}

	private static class ImmediateCopyInstruction<R, V> extends CopyInstruction<R, V> implements Instruction<R, V> {
		private V value;

		public ImmediateCopyInstruction(V value, R into) {
			super(into);
			this.value = value;
		}

		@Override
		protected V getValue(Machine<R, V> machine) {
			return value;
		}
	}

	private abstract static class ModifyInstruction<R, V> implements Instruction<R, V> {
		private R register;

		public ModifyInstruction(R register) {
			this.register = register;
		}

		@Override
		public void runOn(Machine<R, V> machine) {
			machine.modifyRegister(register, this::modify);
			machine.advanceInsructionPointer();
		}

		public abstract V modify(V in);
	}

	private abstract static class ConditionalJumpInstruction<R, V> implements Instruction<R, V> {
		@Override
		public void runOn(Machine<R, V> machine) {
			if (condition(machine)) {
				machine.advanceInsructionPointer(getBy(machine));
			} else {
				machine.advanceInsructionPointer();
			}
		}

		public abstract boolean condition(Machine<R, V> machine);
		public abstract int getBy(Machine<R, V> machine);
	}

	private abstract static class ConditionalRelativeJumpInstruction<R, V> extends ConditionalJumpInstruction<R, V> {
		private int by;

		public ConditionalRelativeJumpInstruction(int by) {
			this.by = by;
		}

		@Override
		public int getBy(Machine<R, V> machine) {
			return by;
		}
	}

	private static class RelativeJumpInstruction<R, V> extends ConditionalRelativeJumpInstruction<R, V> {
		public RelativeJumpInstruction(int by) {
			super(by);
		}

		public boolean condition(Machine<R, V> machine) {
			return true;
		}
	}

	private static abstract class AbstractMachine<R, V> implements Machine<R, V> {
		private int insructionPointer = 0;
		private List<Instruction<R, V>> instructions;

		public AbstractMachine(List<Instruction<R, V>> instructions) {
			this.instructions = instructions;
		}

		public void advanceInsructionPointer() {
			advanceInsructionPointer(1);
		}

		public void advanceInsructionPointer(int by) {
			insructionPointer += by;
		}

		public void run() {
			while(isRunning()) {
				instructions.get(insructionPointer).runOn(this);
			}
		}

		protected boolean isRunning() {
			return insructionPointer < instructions.size();
		}
	}

	private static class DefaultMachine extends AbstractMachine<DefaultMachine.Register, Integer> {
		public static enum Register { A,B,C,D }

		private static final Register INPUT_REGISTER = Register.C;
		private static final Register RESULT_REGISTER = Register.A;

		private final Map<Register, Integer> registers = new EnumMap<>(Register.class);
		{
			for (Register r : Register.values()) {
				registers.put(r, 0);
			}
		}

		public DefaultMachine(List<Instruction<Register, Integer>> instructions) {
			super(instructions);
		}

		public DefaultMachine(List<Instruction<Register, Integer>> instructions, int input) {
			this(instructions);
			registers.put(INPUT_REGISTER, input);
		}

		public Integer getRegister(Register r) {
			return registers.get(r);
		}

		public void setRegister(Register r, Integer value) {
			registers.put(r, value);
		}

		public Integer getResult() {
			if (isRunning()) throw new IllegalStateException("Can not get result before running");
			return registers.get(RESULT_REGISTER);
		}
	}

	private static class DefaultInstructionSet {
		private static final Pattern REGISTER_PATTERN = Pattern.compile("[abcd]");

		public List<Instruction<DefaultMachine.Register, Integer>> parseFile(File file) throws FileNotFoundException {
			try(Scanner scanner = new Scanner(file)) {
				final List<Instruction<DefaultMachine.Register, Integer>> instructions = new ArrayList<>();

				while(scanner.hasNext()) {
					final String opCode = scanner.next();
					switch (opCode) {
					case "cpy": instructions.add(parseCopy(scanner)); break;
					case "inc": instructions.add(parseAdd(scanner, 1)); break;
					case "dec": instructions.add(parseAdd(scanner, -1)); break;
					case "jnz": instructions.add(parseJumpNonZero(scanner)); break;
					default: throw new RuntimeException("Invalid OpCode: " + opCode);
					}
				}

				return instructions;
			}

		}

		CopyInstruction<DefaultMachine.Register, Integer> parseCopy(Scanner scanner) {
			return (parseRegister(scanner)
					.map(from -> (Function<DefaultMachine.Register, CopyInstruction<DefaultMachine.Register, Integer>>)
							to -> new RegisterCopyInstruction<>(from, to)))
					.orElseGet(() -> (
						parseImmediate(scanner)
							.map(value -> (Function<DefaultMachine.Register, CopyInstruction<DefaultMachine.Register, Integer>>)
									to -> new ImmediateCopyInstruction<>(value, to))
							.orElseThrow(() -> new RuntimeException("Expecting register or immediate operand"))
					)).apply(parseRegisterOrThrow(scanner));
		}

		AddInstruction<DefaultMachine.Register> parseAdd(Scanner scanner, Integer offset) {
			return new AddInstruction<DefaultMachine.Register>(parseRegisterOrThrow(scanner), offset);
		}

		private static class AddInstruction<R> extends ModifyInstruction<R, Integer> {
			private Integer offset;

			public AddInstruction(R register, Integer offset) {
				super(register);
				this.offset = offset;
			}

			@Override
			public Integer modify(Integer in) {
				return in + offset;
			}

		}

		ConditionalJumpInstruction<DefaultMachine.Register, Integer> parseJumpNonZero(Scanner scanner) {
			return (parseRegister(scanner)
					.map(register -> (Function<Integer, ConditionalJumpInstruction<DefaultMachine.Register, Integer>>)
							offset -> new NonZeroJumpInstruction<>(register, offset)))
					.orElseGet(() -> (
						parseImmediate(scanner)
							.map(value -> (Function<Integer, ConditionalJumpInstruction<DefaultMachine.Register, Integer>>)
									offset -> new RelativeJumpInstruction<>(value != 0 ? offset : 1))
							.orElseThrow(() -> new RuntimeException("Expecting register or immediate operand"))
					)).apply(parseImmediateOrThrow(scanner));
		}

		private static class NonZeroJumpInstruction<R> extends ConditionalRelativeJumpInstruction<R, Integer> {
			private R register;

			public NonZeroJumpInstruction(R register, Integer by) {
				super(by);
				this.register = register;
			}

			@Override
			public boolean condition(Machine<R, Integer> machine) {
				return machine.getRegister(register) != 0;
			}
		}

		Optional<DefaultMachine.Register> parseRegister(Scanner scanner) {
			if (scanner.hasNext(REGISTER_PATTERN)) {
				return Optional.of(DefaultMachine.Register.values()[scanner.next().charAt(0) - 'a']);
			} else {
				return Optional.empty();
			}

		}

		DefaultMachine.Register parseRegisterOrThrow(Scanner scanner) {
			return parseRegister(scanner).orElseThrow(() -> new RuntimeException("Expecting register"));
		}

		Optional<Integer> parseImmediate(Scanner scanner) {
			if (scanner.hasNextInt()) {
				return Optional.of(scanner.nextInt());
			} else {
				return Optional.empty();
			}
		}

		Integer parseImmediateOrThrow(Scanner scanner) {
			return parseImmediate(scanner).orElseThrow(() -> new RuntimeException("Expecting immediate"));
		}

	}

	public static void main(String[] args) throws FileNotFoundException {
		final List<Instruction<DefaultMachine.Register, Integer>> instructions
			= new DefaultInstructionSet().parseFile(new File("input.txt"));
		{
			final Machine<DefaultMachine.Register, Integer> m = new DefaultMachine(instructions);
			m.run();
			System.out.println(m.getResult());
		}
		{
			final Machine<DefaultMachine.Register, Integer> m = new DefaultMachine(instructions, 1);
			m.run();
			System.out.println(m.getResult());
		}
	}
}
