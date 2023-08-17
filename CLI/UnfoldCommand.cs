using System.CommandLine;

namespace CLI;

internal static class UnfoldCommand
{
    public static void AddUnfoldCommand(this RootCommand rootCommand)
    {
        var arg = new Argument<FileInfo>("specification")
        {
            Description = "The file to process"
        };
        var unfoldCommand = new Command("unfold")
        {
            Description = $"Parse input specification (<{arg.Name}>), " +
                          $"unfold and output the unfolded specification " +
                          $"in a new file"
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
            var (schemeName, declarations) = astOption.Value;

            var (_, unfoldedDeclarations) = Transpiler.Transpiler.transpile(schemeName, declarations);


            Transpiler.Writer.write(schemeName, unfoldedDeclarations,
                $"{file.Directory?.FullName}/{specificationName}_unfolded{extension}");
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
