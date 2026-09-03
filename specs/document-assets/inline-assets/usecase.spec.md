---
id: DOC-ASSET-INLINE-001
type: use-case
parent: DOC-ASSET
title: "Render declared asset inline"
status: review
change_class: additive
actors: [Library author, Documentation reader]
emits: [AssetRegistered, AssetRendered]
consumes: [AssetDeclaration, DocumentationSource]
owners:
  analyst: "@codex"
  developer: "@codex"
  tester: "@codex"
tags: [assets, inline-image, html, markdown]
created: 2026-09-03
updated: 2026-09-03
---

## § Intent

A library author declares an image once and places its name in documentation so a documentation reader sees that image in the rendered format. If the declaration cannot be resolved safely, rendering stops instead of emitting a broken image.

## § Domain Rules

- **DR-1**: A declared asset name resolves to exactly one existing source file. A missing source fails rendering and identifies the name.
- **DR-2**: A declared asset name in documentation becomes an image in each supported output format. It must not be rendered as text or a navigation link.
- **DR-3**: The image file is copied to one safe, unique path below the output root. A path collision or traversal attempt fails rendering.
- **DR-4**: Image source paths are relative to the page that contains the image, including nested pages.
- **DR-5**: Repeated occurrences of the same asset do not duplicate its output file.

## § Acceptance Criteria

```gherkin
Scenario: Render a declared image in HTML (→ DR-1, DR-2, DR-3)
  Given an author has declared an existing asset named @DEMO.GIF
    And a documentation page contains @DEMO.GIF
  When the author renders HTML documentation
  Then the page contains an image for @DEMO.GIF
    And its source file exists below the output root

Scenario: Render a declared image in Markdown (→ DR-2, DR-3)
  Given an author has declared an existing asset named @DEMO.GIF
    And a documentation page contains @DEMO.GIF
  When the author renders Markdown documentation
  Then the page contains image markup for @DEMO.GIF
    And its source file exists below the output root

Scenario: Render the image from a nested page (→ DR-4)
  Given an author has declared an existing asset named @DEMO.GIF
    And a page two directories below the output root contains @DEMO.GIF
  When the author renders documentation
  Then the emitted image source reaches the copied file using a relative path

Scenario: Render a repeated image (→ DR-5)
  Given an author has declared an existing asset named @DEMO.GIF
    And two documentation pages contain @DEMO.GIF
  When the author renders documentation
  Then one output file is created for @DEMO.GIF

Scenario: Reject an unusable declaration (→ DR-1, DR-3)
  Given an author has declared an asset with an absent source or unsafe output path
  When the author renders documentation containing its name
  Then rendering fails with an error that identifies the asset and reason
```

## § Domain Model Touch

- **Aggregate**: Asset registry (create and query)
- **Events emitted**: AssetRegistered, AssetRendered
- **Events consumed**: AssetDeclaration, DocumentationSource
- **Invariants**: declared source exists; target is safe and unique; rendered reference is an image

## § Constraints

- **[COMPAT]** Existing image syntax and regular documentation references remain unchanged.
- **[IDMP]** Copying the same registered asset is idempotent per output directory.
- **[SEC]** No asset declaration can write outside the selected output root.
- **[PERF]** Resolution of a declared asset does not scan the full declaration set.

## § Open Questions

None.

## § Decision Log

- **DL-1** (2026-09-03): The source-path-derived output path is the v1 default, avoiding an additional public option until a concrete need arises.
- **DL-2** (2026-09-03): Alt text defaults to the asset symbol's name and can be expanded in a follow-up feature if richer metadata is required.
