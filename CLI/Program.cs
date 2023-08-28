using System.CommandLine;

namespace CLI;

internal class Program
{
    public static readonly Option<bool> DebugOption = new(
        new[] {"--debug", "-d"},
        getDefaultValue: () => false,
        description: "Enable debug mode");

    private static async Task<int> Main(string[] args)
    {
        var rootCommand =
            new RootCommand(
                "`rslts` is the RAISE Specification Language toolset. This can be used to unfold specifications and render their AST");
        rootCommand.AddOption(DebugOption);

        rootCommand.AddUnfoldCommand();
        rootCommand.AddAstCommand();


        return await rootCommand.InvokeAsync(args);
    }
}
