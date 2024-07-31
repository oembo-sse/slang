// @ts-check

import * as monaco from "./lib/monaco/editor.api.js";

export const LANGUAGE_ID = "slang";

monaco.languages.register({ id: LANGUAGE_ID });

monaco.languages.setLanguageConfiguration(LANGUAGE_ID, {
  comments: {
    lineComment: "//",
    blockComment: ["/*", "*/"],
  },
  brackets: [
    ["(", ")"],
    ["{", "}"],
    ["[", "]"],
  ],
  autoClosingPairs: [
    { open: "[", close: "]" },
    { open: "{", close: "}" },
    { open: "(", close: ")" },
    { open: "'", close: "'", notIn: ["string", "comment"] },
    { open: '"', close: '"', notIn: ["string"] },
  ],
  surroundingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
    { open: '"', close: '"' },
    { open: "'", close: "'" },
  ],
  folding: {
    markers: {
      start: new RegExp("^\\s*#pragma\\s+region\\b"),
      end: new RegExp("^\\s*#pragma\\s+endregion\\b"),
    },
  },
  wordPattern: /[a-zA-Z_@$ΣΛλ][a-zA-Z0-9_]*/,
});

monaco.languages.setMonarchTokensProvider(LANGUAGE_ID, {
  brackets: [
    { token: "delimiter.curly", open: "{", close: "}" },
    { token: "delimiter.parenthesis", open: "(", close: ")" },
    { token: "delimiter.square", open: "[", close: "]" },
    { token: "delimiter.angle", open: "<", close: ">" },
  ],

  keywords: [
    "method",
    "function",
    "domain",
    "global",
    "axiom",

    "requires",
    "ensures",
    "modifies",

    "var",
    "match",
    "loop",
    "for",
    "in",
    "break",
    "continue",
    "return",
    "assert",
    "assume",
  ],
  constants: ["true", "false", "forall", "exists", "result", "Int", "Bool"],
  operators: [
    "-",
    ",",
    "->",
    "=>",
    "==>",
    ":=",
    "!",
    "!=",
    "(",
    ")",
    "{",
    "}",
    "*",
    "/",
    "^",
    "&&",
    "&",
    "+",
    "<",
    "<=",
    "=",
    ">",
    ">=",
    "||",
    "|",
    "..",
  ],

  tokenizer: {
    root: [
      [
        /[a-zA-Z_@$ΣΛλ][a-zA-Z0-9_]*/,
        {
          cases: {
            "@keywords": "keyword",
            "@constants": "constant.language",
            "@operators": "operator",
            "@default": "identifier",
          },
        },
      ],
      { include: "@whitespace" },
      [/[-,:=!*\/&+<>|\.]/, "keyword.operator"],
      [/(\/\/).*$/, "comment"],
      [/[{}()\[\]]/, "@brackets"],
      [/[0-9]+/, "number"],
    ],
    whitespace: [
      [/[ \t\r\n]+/, ""],
      [/\/\*/, "comment", "@comment"],
      [/\/\/.*\\$/, "comment", "@linecomment"],
      [/\/\/.*$/, "comment"],
    ],
    comment: [
      [/[^\/*]+/, "comment"],
      [/\*\//, "comment", "@pop"],
      [/[\/*]/, "comment"],
    ],
    linecomment: [
      [/.*[^\\]$/, "comment", "@pop"],
      [/[^]+/, "comment"],
    ],
  },
});
