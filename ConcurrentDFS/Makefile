SCALAC = scalac -cp .:/Users/marta/Documents/Oxford/PartA/ConcurrentProgramming

SRC_DIR = .

SCALA_FILES = $(wildcard $(SRC_DIR)/*.scala)

TARGET_DIR = ./bin

CLASS_FILES = $(patsubst $(SRC_DIR)/%.scala,$(TARGET_DIR)/%.class,$(SCALA_FILES))

all: $(CLASS_FILES)

$(TARGET_DIR)/%.class: $(SRC_DIR)/%.scala
	@mkdir -p $(dir $@)
	$(SCALAC) -d $(TARGET_DIR) $<

clean:
	rm -rf $(TARGET_DIR)

.PHONY: all clean
