// See https://aka.ms/new-console-template for more information

using System.CommandLine;

namespace CLI;

internal class Program
{
    static void Main(string[] args)
    {
        var rootCommand = new RootCommand("Let's go!");
        
        rootCommand.SetHandler(() =>
        {
            Console.WriteLine("Hello from root");
        });
        
        var sub1Command = new Command("sub1", "First-level subcommand");
        rootCommand.Add(sub1Command);
        
        sub1Command.SetHandler(() =>
        {
            Console.WriteLine("Let's go - Hello world!");
        });
        
        
        var sub1aCommand = new Command("sub1a", "Second level subcommand");
        rootCommand.Add(sub1aCommand);
    }
}