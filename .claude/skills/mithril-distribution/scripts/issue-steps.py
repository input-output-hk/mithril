"""Lists the How checkboxes of a release issue phase, and renders a body with some of them ticked.

Read-only: reads the issue with `gh issue view`, writes the new body only to a local file.
Rights: public read of the issue; the printed `gh issue edit` needs the Triage or Write role on the repository.

Usage:
  python3 -I issue-steps.py <issue> <phase-number> [--json]
  python3 -I issue-steps.py <issue> <phase-number> --tick <index>[,<index>...] --out <file>
  python3 -I issue-steps.py <issue> <phase-number> --strike <index>[,<index>...] --out <file>
  Any form takes --in <file> to read the body from a local file (to chain edits) instead of the issue.
  A form with --out takes --print-command to print the changed lines and the `gh issue edit` command, guarded by --unchanged.
  python3 -I issue-steps.py <issue> --unchanged <sha256>   Exit 1 when the issue body changed since it was rendered.
Requirements: python3, gh (unless every read uses --in).
  python3 -I issue-steps.py <issue> --scope <distribution> [--json]
  python3 -I issue-steps.py <issue> [<phase-number> --tick <index>[,...]] --scope <distribution> --scope-tick <index>[,...] --out <file>
"""

import argparse
import hashlib
import json
import os
import re
import shlex
import shutil
import subprocess
import sys

CHECKBOX = re.compile(r"^(?P<indent>[ >]*?)(?P<bullet>[-*]) \[(?P<mark>[ xX])\] (?P<text>.*)$")
REPO = os.environ.get("MITHRIL_REPO", "IntersectMBO/mithril")


def read_body(issue, source):
    if source:
        with open(source, encoding="utf-8") as handle:
            body = handle.read()
    else:
        body = subprocess.run(
            [require_gh(), "issue", "view", str(issue), "--repo", REPO, "--json", "body", "--jq", ".body"],
            check=True,
            capture_output=True,
            text=True,
        ).stdout
    return body.replace("\r\n", "\n").rstrip("\n").split("\n")


def require_gh():
    if shutil.which("gh") is None:
        sys.exit("Missing required command: gh")
    return "gh"


def body_hash(lines):
    return hashlib.sha256("\n".join(lines).encode("utf-8")).hexdigest()


def section_bounds(lines, heading_pattern, level):
    start = None
    for index, line in enumerate(lines):
        if start is None:
            if re.match(heading_pattern, line):
                start = index + 1
        elif re.match(r"^#{1,%d} " % level, line):
            return start, index
    if start is None:
        return None
    return start, len(lines)


def parse_steps(lines, start, end):
    steps = []
    parents = []
    for line_index in range(start, end):
        match = CHECKBOX.match(lines[line_index])
        if not match:
            continue
        indent = re.sub(r"^> ?", "", match.group("indent"))
        depth = len(indent) // 2
        while parents and parents[-1]["depth"] >= depth:
            parents.pop()
        step = {
            "index": len(steps) + 1,
            "line": line_index,
            "depth": depth,
            "checked": match.group("mark") in "xX",
            "text": match.group("text").strip(),
            "parent": parents[-1]["index"] if parents else None,
        }
        steps.append(step)
        parents.append(step)
    for step in steps:
        step["children"] = [child["index"] for child in steps if child["parent"] == step["index"]]
    return steps


def phase_steps(lines, phase):
    how = section_bounds(lines, r"^## How\b", 2)
    if how is None:
        sys.exit("No '## How' section in the issue")
    sub_lines = lines[how[0]:how[1]]
    bounds = section_bounds(sub_lines, r"^### Phase %s\b" % re.escape(phase), 3)
    if bounds is None:
        sys.exit("No '### Phase %s' subsection in the How section" % phase)
    return parse_steps(lines, how[0] + bounds[0], how[0] + bounds[1])


def print_steps(steps, as_json):
    if as_json:
        print(json.dumps(steps, indent=2))
        return
    for step in steps:
        mark = "🟢" if step["checked"] else "🔴"
        print("%3d %s%s %s" % (step["index"], "  " * step["depth"], mark, step["text"]))


def tick(lines, steps, indexes):
    by_index = {step["index"]: step for step in steps}
    for index in indexes:
        if index not in by_index:
            sys.exit("Unknown step index %d" % index)
        line = by_index[index]["line"]
        lines[line] = re.sub(r"\[ \]", "[x]", lines[line], count=1)


def strike(lines, steps, indexes):
    by_index = {step["index"]: step for step in steps}
    for index in indexes:
        if index not in by_index:
            sys.exit("Unknown step index %d" % index)
        step = by_index[index]
        match = CHECKBOX.match(lines[step["line"]])
        text = step["text"]
        if not (text.startswith("~~") and text.endswith("~~")):
            text = "~~%s~~" % text
        lines[step["line"]] = "%s%s [x] %s" % (match.group("indent"), match.group("bullet"), text)


def scope_steps(lines, distribution):
    what = section_bounds(lines, r"^## What\b", 2)
    if what is None:
        sys.exit("No '## What' section in the issue")
    sub_lines = lines[what[0]:what[1]]
    bounds = section_bounds(sub_lines, r"^### Distribution %s\s*:?\s*$" % re.escape(distribution), 3)
    if bounds is None:
        sys.exit("No '### Distribution %s' subsection in the What section" % distribution)
    return parse_steps(lines, what[0] + bounds[0], what[0] + bounds[1])


def parse_indexes(value):
    return [int(index) for index in value.split(",")]


def print_command(issue, original, lines, out, rendered_from):
    for before, after in zip(original, lines):
        if before != after:
            print(after.lstrip(" >"))
    print()
    guard = shlex.join(["python3", "-I", os.path.abspath(__file__), str(issue), "--unchanged", body_hash(rendered_from)])
    print(f"{guard} && gh issue edit {issue} --repo {REPO} --body-file {shlex.quote(out)}")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("issue")
    parser.add_argument("phase", nargs="?")
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--tick")
    parser.add_argument("--strike")
    parser.add_argument("--out")
    parser.add_argument("--in", dest="source")
    parser.add_argument("--scope")
    parser.add_argument("--scope-tick")
    parser.add_argument("--print-command", action="store_true")
    parser.add_argument("--unchanged", metavar="SHA256")
    arguments = parser.parse_args()

    if arguments.unchanged:
        if body_hash(read_body(arguments.issue, None)) != arguments.unchanged:
            sys.exit("Issue %s changed since its new body was rendered, render it again" % arguments.issue)
        return

    if (arguments.tick or arguments.strike or arguments.scope_tick) and not arguments.out:
        parser.error("--tick, --strike and --scope-tick require --out")
    if (arguments.tick or arguments.strike) and not arguments.phase:
        parser.error("--tick and --strike require a phase number")
    if arguments.scope_tick and not arguments.scope:
        parser.error("--scope-tick requires --scope")
    if not arguments.phase and not arguments.scope:
        parser.error("phase number or --scope is required")

    lines = read_body(arguments.issue, arguments.source)
    original = list(lines)
    if arguments.tick or arguments.strike or arguments.scope_tick:
        if arguments.tick:
            tick(lines, phase_steps(lines, arguments.phase), parse_indexes(arguments.tick))
        if arguments.strike:
            strike(lines, phase_steps(lines, arguments.phase), parse_indexes(arguments.strike))
        if arguments.scope_tick:
            tick(lines, scope_steps(lines, arguments.scope), parse_indexes(arguments.scope_tick))
        with open(arguments.out, "w", encoding="utf-8") as handle:
            handle.write("\n".join(lines) + "\n")
        if arguments.print_command:
            rendered_from = original if not arguments.source else read_body(arguments.issue, None)
            print_command(arguments.issue, original, lines, arguments.out, rendered_from)
        return
    if arguments.scope:
        print_steps(scope_steps(lines, arguments.scope), arguments.json)
        return
    print_steps(phase_steps(lines, arguments.phase), arguments.json)


if __name__ == "__main__":
    main()
