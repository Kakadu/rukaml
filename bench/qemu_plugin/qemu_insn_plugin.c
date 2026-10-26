/*
 * qemu_insn_plugin.c - count guest instructions executed by a qemu-user run.
 *
 * The count is written as a decimal integer to the file named by the
 * QEMU_INSN_FILE environment variable when qemu exits.  bench/harness.c sets
 * that variable, runs qemu with `-plugin <this .so>`, and reads the file.
 *
 * Build (see the root Makefile):
 *   cc -O2 -shared -fPIC $(pkg-config --cflags glib-2.0) \
 *      -o bench/qemu_plugin/libqemu_insn.so bench/qemu_plugin/qemu_insn_plugin.c
 */

#include <qemu-plugin.h>

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

QEMU_PLUGIN_EXPORT int qemu_plugin_version = QEMU_PLUGIN_VERSION;

static struct qemu_plugin_scoreboard *g_score;
static qemu_plugin_u64 g_insns;

static void tb_trans(struct qemu_plugin_tb *tb, void *userdata) {
  (void)userdata;
  size_t n = qemu_plugin_tb_n_insns(tb);
  for (size_t i = 0; i < n; ++i) {
    struct qemu_plugin_insn *insn = qemu_plugin_tb_get_insn(tb, i);
    qemu_plugin_register_vcpu_insn_exec_inline_per_vcpu(
        insn, QEMU_PLUGIN_INLINE_ADD_U64, g_insns, 1);
  }
}

static void at_exit(void *userdata) {
  (void)userdata;
  uint64_t total = qemu_plugin_u64_sum(g_insns);
  const char *path = getenv("QEMU_INSN_FILE");
  if (path != NULL && *path != '\0') {
    FILE *f = fopen(path, "w");
    if (f != NULL) {
      fprintf(f, "%" PRIu64 "\n", total);
      fclose(f);
      return;
    }
  }
  qemu_plugin_outs("qemu_insn_plugin: QEMU_INSN_FILE is not writable\n");
}

QEMU_PLUGIN_EXPORT int qemu_plugin_install(qemu_plugin_id_t id,
                                           const qemu_info_t *info, int argc,
                                           char **argv) {
  (void)info;
  (void)argc;
  (void)argv;
  g_score = qemu_plugin_scoreboard_new(sizeof(uint64_t));
  if (g_score == NULL) {
    qemu_plugin_outs("qemu_insn_plugin: cannot allocate scoreboard\n");
    return -1;
  }
  g_insns = qemu_plugin_scoreboard_u64(g_score);
  qemu_plugin_register_vcpu_tb_trans_cb(id, tb_trans, NULL);
  qemu_plugin_register_atexit_cb(id, at_exit, NULL);
  return 0;
}
