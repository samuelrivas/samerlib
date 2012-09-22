REBAR_REPO = https://github.com/basho/rebar.git
REBAR_VERSION = 2.0.0
REBAR_REPO_DIR = rebar
REBAR = $(REBAR_REPO_DIR)/rebar

all: compile

$(REBAR): $(REBAR_REPO_DIR)
	cd $(REBAR_REPO_DIR); ./bootstrap

$(REBAR_REPO_DIR):
	git clone $(REBAR_REPO) $(REBAR_REPO_DIR)

get-deps: $(REBAR)
	$(REBAR) get-deps

compile: get-deps
	$(REBAR) compile

check: compile
	$(REBAR) xref

test: compile
	$(REBAR) eunit skip_deps=true

clean:
	$(REBAR) clean

clean-all: clean
	$(REBAR) delete-deps
	rm -rf $(REBAR_REPO_DIR)
