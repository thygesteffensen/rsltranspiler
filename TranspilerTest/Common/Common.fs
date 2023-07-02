module TranspilerTest.Common

open FSharp.Text.Lexing

/// <summary>
/// Build <see cref="FSharp.Text.Lexing.Position"/> based on input.
/// </summary>
/// <param name="pos_lnum">Line number</param>
/// <param name="pos_cnum">Column number</param>
/// <param name="pos_bol">Absolute character index</param>
/// <param name="pos_fname">File name</param>
let pos (pos_lnum: int) (pos_cnum: int) (pos_bol: int) (pos_fname: string) =
    { pos_bol = pos_bol
      pos_cnum = pos_cnum
      pos_orig_lnum = pos_lnum
      pos_fname = pos_fname
      pos_lnum = pos_lnum }
