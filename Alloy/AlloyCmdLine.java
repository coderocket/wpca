/* Alloy Analyzer 4 -- Copyright (c) 2006-2009, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

import java.io.File;
import java.io.PrintStream;
import java.io.FileNotFoundException;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Module;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;
import edu.mit.csail.sdg.alloy4viz.VizGUI;
import edu.mit.csail.sdg.alloy4.Version;

/** This class demonstrates how to access Alloy4 via the compiler methods. */

public final class AlloyCmdLine {

    /*
     * Execute all the commands in the first file and place any counterexamples 
     * into the output file.
     *
     * If there are syntax or type errors, it may throw
     * a ErrorSyntax or ErrorType or ErrorAPI or ErrorFatal exception.
     * You should catch them and display them,
     * and they may contain filename/line/column information.
     */
    public static void main(String[] args) throws Err, FileNotFoundException {

	if (args.length != 2) {
		System.out.print("Usage: <alloy file> <output file>\n");
		System.exit(1);
	}

        // The visualizer (We will initialize it to nonnull when we visualize an Alloy solution)
        VizGUI viz = null;

        // Alloy4 sends diagnostic messages and progress reports to the A4Reporter.
        // By default, the A4Reporter ignores all these events (but you can extend the A4Reporter to display the event for the user)
        A4Reporter rep = new A4Reporter() {
            // For example, here we choose to display each "warning" by printing it to System.out
            @Override public void warning(ErrorWarning msg) {
                System.out.print("Relevance Warning:\n"+(msg.toString().trim())+"\n\n");
                System.out.flush();
            }
 	    @Override public void solve(int primaryVars, int totalVars, int clauses) {
                System.out.println("... solving (" + Integer.toString(primaryVars) + " primary variables)");
                System.out.flush();
	    }
 	    @Override public void translate(java.lang.String solver, int bitwidth, int maxseq, int skolemDepth, int symmetry)  {
                System.out.println("... generating CNF for " + solver + " (" + Integer.toString(bitwidth) + " bitwidth)");
                System.out.flush();
	    }
        };

	String filename = args[0];
	PrintStream output = new PrintStream(new File(args[1]));

	// Identify yourself
	System.out.println("Alloy Analyzer " + Version.version() + " (build: " + Version.buildNumber() +" date: " + Version.buildDate() +")");

	try {
	        // Parse+typecheck the model
	        System.out.println("Parsing and Typechecking "+filename+" ...");
	        Module world = CompUtil.parseEverything_fromFile(rep, null, filename);
	
	        // Choose some default options for how you want to execute the commands
	        A4Options options = new A4Options();
	        options.solver = A4Options.SatSolver.SAT4J;
		boolean foundCounterExamples = false;
	        for (Command command: world.getAllCommands()) {
	            // Execute the command
	            System.out.println("... Executing command "+command+":");
	            A4Solution ans = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), command, options);
	            // Print the outcome
	            output.println(command+":");
	            output.println(ans);
	            // If satisfiable...
	            if (ans.satisfiable()) {
			foundCounterExamples = true;
	                // You can query "ans" to find out the values of each set or type.
	                // This can be useful for debugging.
	                //
	                // You can also write the outcome to an XML file
	                // ans.writeXML("alloy_example_output.xml");
	                //
	                // You can then visualize the XML file by calling this:
	/*
	                    if (viz==null) {
	                        viz = new VizGUI(false, "alloy_example_output.xml", null);
	                    } else {
	                        viz.loadXML("alloy_example_output.xml", true);
	                    }
	*/
	            }
	        }
		if (!foundCounterExamples)
			System.out.println("\nFound no counterexamples, procedure may be correct.");
        }
        catch(Err error) {
            System.out.println("Alloy has encountered an error at: " + error.pos.toString() +": " + error.msg);
        }
    }
}

