srcdir := $(CURDIR)
objdir := $(CURDIR)

SRCS := ir_builder.cc
SRCS_FILES := $(SRCS)

OBJS := $(patsubst %.cc,%.op,$(SRCS_FILES))
OBJS := $(addprefix $(objdir)/,$(OBJS))

CXXFLAGS += -g $(shell llvm-config --cppflags) -I$(LIB_SRCS) -fPIC
TARGET := $(notdir $(LIBLLVM_ARCHIVE))

all: $(TARGET)

$(OBJS):
	$(CXX) -c $(@:.op=.cc) -o $@ $(CXXFLAGS) -I$(LIBDEX_BASE) -I$(LIBARTBASE_BASE) -I$(LIBBASE_HDRS) -I$(JNI_HDRS)

$(TARGET): $(OBJS)
	$(QUIET_AR)ar -rcs $@ $^

clean:
	find . -name '*.o' -exec rm {} \;
	rm -f $(TARGET)
