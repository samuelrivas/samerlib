REBAR_REPO = https://github.com/basho/rebar.git
REBAR_VERSION = 2.0.0
REBAR_REPO_DIR = rebar
REBAR = $(REBAR_REPO_DIR)/rebar

all: compile

$(REBAR): $(REBAR_REPO_DIR)
	cd $(REBAR_REPO_DIR); ./bootstrap

$(REBAR_REPO_DIR):
	git clone $(REBAR_REPO) $(REBAR_REPO_DIR)

compile: $(REBAR)
	$(REBAR) compile

check: compile
	$(REBAR) xref

clean:
	$(REBAR) clean

clean-all: clean
	rm -rf $(REBAR_REPO_DIR)
