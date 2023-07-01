using System.CommandLine;
using System.Diagnostics;

namespace CLI;

internal static class AstCommand
{
    public static void AddAstCommand(this RootCommand rootCommand)
    {
        var arg = new Argument<FileInfo>("specification")
        {
            Description = "The file to process"
        };
        var unfoldCommand = new Command("ast")
        {
            Description = $"Parse input specification (<{arg.Name}>). and output rendered AST. (This command uses locally installed GhostScript tool `gs`)"
        };

        unfoldCommand.SetHandler(UnfoldSpecification, Program.DebugOption, arg);
        unfoldCommand.AddArgument(arg);

        rootCommand.AddCommand(unfoldCommand);
    }

    private static void UnfoldSpecification(bool debug, FileInfo? file)
    {
        if (file == null || !file.Exists)
        {
            Console.WriteLine($"Cannot find {file?.Name}");
            return;
        }

        if (debug)
            Console.WriteLine($"Trying to unfold {file.Name}");

        var specificationName = Path.GetFileNameWithoutExtension(file.FullName);
        var extension = Path.GetExtension(file.FullName);

        var astOption = Transpiler.Reader.testLexerAndParserFromFile(file.Name);

        try
        {
            // astOption.Value throws NUllReferenceException if astOption is None.
            var (schemeName, declarations) = astOption.Value;
            Transpiler.AstDrawer.schemeToTree(schemeName, declarations, $"{specificationName}.ps");

            var process = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = "gs",
                    Arguments =
                        $"-dSAFER -dBATCH -dNOPAUSE -sDEVICE=png16m -r300 -sOutputFile={specificationName}.png {specificationName}.ps",
                    RedirectStandardOutput = true,
                    UseShellExecute = false,
                    CreateNoWindow = true,
                }
            };

            process.Start();
            while (!process.StandardOutput.EndOfStream)
            {
                var line = process.StandardOutput.ReadLine();
                Console.WriteLine(line);
            }
        }
        catch (NullReferenceException)
        {
            Console.WriteLine("An error occured in the transpiler.");
        }


        if (debug)
            Console.WriteLine(
                $"Unfolded {specificationName} to {file.Directory?.FullName}/{specificationName}_unfolded{extension}");
    }
}