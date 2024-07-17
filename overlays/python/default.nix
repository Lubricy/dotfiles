_:
self: super: {
  open-interpreter = super.open-interpreter.overrideAttrs({pname, ...}:
    let version = "0.3.4";
    in {
      version = version;
      src = super.fetchFromGitHub {
        owner = "OpenInterpreter";
        repo = pname;
        rev = "v${version}";
        hash = "sha256-8zywEtAaAwaTxI6NLBWt3pj2PDJ8KZ0dK6KVVrT4M0M=";
      };
    });
}
