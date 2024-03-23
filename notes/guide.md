## Code:

- Each file should do a narrow, well-defined, well-delineated thing.
- Interactive iteration on a file is allowed, but when a file is ready to be checked in as something official, ideally it should flow like a script.
  No file should read like a sandbox for trial-and-error.
- Better yet, if you want to document the work you do to iterate on some ideas, you can organize that work as "notebook" style document and put it in a "reports/" file.
  This helps you organize each exploratory analysis with the specific questions you were asking and what you found.
- Strive for modularity.
  This means that we want "separate components" that can fit together in any order, _not_ a sequence of "steps" that must be run in a specific order.
  The more the project is organized as a sequence of steps, the more brittle the overall project becomes.
- The model should be easy to implement for any group, or any dataset w/ the same structure.
  If it is not, you are making it hard for you and anybody you want to implement this analysis.


# exporting data to .qmd

- Only export objects (values).
  Never export an entire environment.
  Environments are state (dangerous) but values are pure / side-effect free (safe).
- The narrative flow of the writing should not flow upstream into the code.
  The code should be general and re-usable.
  The code needs to be "made specific" only when shaping some results into a narritive, which happens in the paper.


## Writing

- Try to write sentences on separate lines.
  This is much more convenient for tracking changes, and tracking changes is important.
- Quarto is very general; it supports LaTeX and HTML output.
- It is recommended to preview the paper using static HTML document or serving the HTML.
  PDF renders are slower.
- Iterating with HTML output also prevents you from doing too much LaTeX-specific hacking in the document.
  LaTeX is very flexible, but the more special LaTeX you use, the harder it is for your document to inter-operate.


